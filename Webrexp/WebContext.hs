{-# LANGUAGE ScopedTypeVariables #-}
-- | This module define the state carryied during the webrexp
-- evaluation. This state is implemented as a monad transformer
-- on top of 'IO'.
module Webrexp.WebContext
    ( 
    -- * Types
      WebCrawler
    , WebContextT ( .. )
    , WebContext 
    , NodeContext (..)
    , EvalState (..)
    , BinBlob (..)
    , Context

    -- * Crawling configuration
    , LogLevel (..)
    , setLogLevel 
    , getUserAgent 
    , setUserAgent 
    , setOutput 
    , getOutput
    , getHttpDelay 
    , setHttpDelay 
    , isVerbose

    -- * User function
    , evalWithEmptyContext
    
    -- * Implementation info
    -- ** Evaluation function
    , prepareLogger 

    -- ** State manipulation functions
    , pushCurrentState 
    , popCurrentState 

    -- * DFS evaluator
    -- ** Node list
    , recordNode 
    , popLastRecord 

    -- ** Branch context
    , pushToBranchContext 
    , popBranchContext 
    , addToBranchContext 

    -- ** Unicity manipulation function
    , setBucketCount 
    , incrementGetRangeCounter 
    , hasResourceBeenVisited
    , setResourceVisited
    )
    where

import System.IO
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Array.IO
import qualified Data.Set as Set

import qualified Data.ByteString.Lazy as B
import Webrexp.GraphWalker

-- | Typical use of the WebContextT monad transformer
-- allowing to download information
type WebCrawler node rezPath a = WebContextT node rezPath IO a

-- | WebContext is 'WebContextT' as a simple Monad
type WebContext node rezPath a = WebContextT node rezPath Identity a

-- | Represent a graph node and the path
-- used to go up to it.
data NodeContext node rezPath = NodeContext
    { -- | A path from the lass indirect node to this one
      -- The parent node and the index of this node in the
      -- parent's children is stored.
      parents :: [(node, Int)] 

      -- | Real node value
    , this :: node              

      -- | The last indirect path used to get to this node.
    , rootRef :: rezPath       
    }

-- | Represent a binary blob, often downloaded.
data BinBlob rezPath = BinBlob
    { -- | The last indirect path used to get to this blob.
      sourcePath :: rezPath

      -- | The binary data
    , blobData :: B.ByteString
    }

-- | This type represent the temporary results
-- of the evaluation of regexp.
data EvalState node rezPath =
      Node (NodeContext node rezPath)
    | Text String
    | Blob (BinBlob rezPath)

-- | Type used to represent the current logging level.
-- Default is 'Normal'
data LogLevel = Quiet -- ^ Only display the dumped information
              | Normal -- ^ Display dumped information and IOs
              | Verbose -- ^ Display many debugging information
              deriving (Eq)

-- | Internal data context.
data Context node rezPath = Context
    { -- | Context stack used in breadth-first evaluation
      contextStack :: [[EvalState node rezPath]]

      -- | State waiting to be executed in a depth-
      -- first execution.
    , waitingStates :: [(EvalState node rezPath, Int)]

      -- | State used to implement branches in the depth
      -- first evaluator.
    , branchContext :: [(EvalState node rezPath, Int, Int)]

      -- | Buckets used for uniqueness pruning, all
      -- evaluation kind.
    , uniqueBucket :: IOArray Int (Set.Set String) 

      -- | Counters used for range evaluation in DFS
    , countBucket :: IOUArray Int Int

      -- | Current log level
    , logLevel :: LogLevel
    , httpDelay :: Int
    , httpUserAgent :: String
    , defaultOutput :: Handle
    }

--------------------------------------------------
----            Monad definitions
--------------------------------------------------
newtype (Monad m) => WebContextT node rezPath m a =
    WebContextT { runWebContextT :: Context node rezPath
                                 -> m (a, Context node rezPath ) }

instance (Functor m, Monad m) => Functor (WebContextT node rezPath m) where
    {-# INLINE fmap #-}
    fmap f a = WebContextT $ \c ->
        fmap (\(a', c') -> (f a', c')) $ runWebContextT a c

instance (Functor m, Monad m) => Applicative (WebContextT node rezPath m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Monad (WebContextT node rezPath m) where
    {-# INLINE return #-}
    return a =
        WebContextT $ \c -> return (a, c)

    {-# INLINE (>>=) #-}
    (WebContextT val) >>= f = WebContextT $ \c -> do
        (val', c') <- val c
        runWebContextT (f val') c'

instance MonadTrans (WebContextT node rezPath) where
    lift m = WebContextT $ \c -> do
        a <- m
        return (a, c)

instance (MonadIO m) => MonadIO (WebContextT node rezPath m) where
    liftIO = lift . liftIO

--------------------------------------------------
----            Context manipulation
--------------------------------------------------

emptyContext :: Context node rezPath
emptyContext = Context
    { contextStack = []
    , waitingStates = []
    , branchContext = []
    , logLevel = Normal
    , httpDelay = 1500
    , httpUserAgent = ""
    , defaultOutput = stdout
    , uniqueBucket = undefined
    , countBucket = undefined
    }

--------------------------------------------------
----            Getter/Setter
--------------------------------------------------

-- | Setter for the wait time between two indirect
-- operations.
--
-- The value is stored but not used yet.
setHttpDelay :: (Monad m) => Int -> WebContextT node rezPath m ()
setHttpDelay delay = WebContextT $ \c ->
        return ((), c{ httpDelay = delay })

-- | return the value set by 'setHttpDelay'
getHttpDelay :: (Monad m) => WebContextT node rezPath m Int
getHttpDelay = WebContextT $ \c -> return (httpDelay c, c)

-- | Define the text output for written text.
setOutput :: (Monad m) => Handle -> WebContextT node rezPath m ()
setOutput handle = WebContextT $ \c ->
        return ((), c{ defaultOutput = handle })

-- | Retrieve the default file output used for text.
getOutput :: (Monad m) => WebContextT node rezPath m Handle
getOutput = WebContextT $ \c -> return (defaultOutput c, c)

-- | Set the user agent which must be used for indirect operations
--
-- The value is stored but not used yet.
setUserAgent :: (Monad m) => String -> WebContextT node rezPath m ()
setUserAgent usr = WebContextT $ \c ->
    return ((), c{ httpUserAgent = usr })

-- | return the value set by 'setUserAgent'
getUserAgent :: (Monad m) => WebContextT node rezPath m String
getUserAgent = WebContextT $ \c -> return (httpUserAgent c, c)

-- | Set the value of the logging level.
setLogLevel :: (Monad m) => LogLevel -> WebContextT node rezPath m ()
setLogLevel lvl = WebContextT $ \c ->
    return ((), c{logLevel = lvl})

-- | Tell if the current 'LoggingLevel' is set to 'Verbose'
isVerbose :: (Monad m) => WebContextT node rezPath m Bool
isVerbose = WebContextT $ \c -> 
    return (logLevel c == Verbose, c)

-- | Internally the monad store a stack of state : the list
-- of currently evaluated 'EvalState'. Pushing this context
-- with store all the current nodes in it, waiting for later
-- retrieval.
pushCurrentState :: (Monad m)
                 => [EvalState node rezPath]
                 -> WebContextT node rezPath m ()
pushCurrentState lst = WebContextT $ \c ->
        return ((), c{ contextStack = lst : contextStack c })

-- | Inverse operation of 'pushCurrentState', retrieve
-- stored nodes.
popCurrentState :: (Monad m)
                => WebContextT node rezPath m [EvalState node rezPath]
popCurrentState = WebContextT $ \c ->
    case contextStack c of
         []     -> error "Empty context stack, implementation bug"
         (x:xs) -> 
            return (x, c{ contextStack = xs })

-- | Helper function used to start the evaluation of a webrexp
-- with a default context, with sane defaults.
evalWithEmptyContext :: (Monad m)
                     => WebContextT node rezPath m a -> m a
evalWithEmptyContext val = do
    (finalVal, _context) <- runWebContextT val emptyContext
    return finalVal

-- | Return normal, error, verbose logger
prepareLogger :: (Monad m)
              => WebContextT node rezPath m (Logger, Logger, Logger)
prepareLogger = WebContextT $ \c ->
    let silenceLog = \_ -> return ()
        errLog = hPutStrLn stderr
        normalLog = hPutStrLn stdout
    in case logLevel c of
      Quiet -> return ((silenceLog, errLog, silenceLog), c)
      Normal -> return ((normalLog, errLog, silenceLog), c)
      Verbose -> return ((normalLog, errLog, normalLog), c)

--------------------------------------------------
----            Depth First evaluation
--------------------------------------------------

-- | Record a node in the context for the DFS evaluation.
recordNode :: (Monad m)
           => (EvalState node rezPath, Int) -> WebContextT node rezPath m ()
recordNode n = WebContextT $ \c ->
    return ((), c{ waitingStates = n : waitingStates c })

-- | Get the last record from the top of the stack
popLastRecord :: (Monad m)
              => WebContextT node rezPath m (EvalState node rezPath, Int)
popLastRecord = WebContextT $ \c ->
    case waitingStates c of
      [] -> error "popLAst Record - Empty stack!!!"
      (x:xs) -> return (x, c{ waitingStates = xs })


-- | Add a \'frame\' context to the current DFS evaluation.
-- A frame context possess a node to revert to and two counters.
--
--  * A counter for seen nodes which must be evaluated before
--    backtracking
--
--  * A counter for valid node count, to keep track if the whole
--    frame has a valid result or not.
--
-- You can look at 'popBranchContext' and 'addToBranchContext'
-- for other frame manipulation functions.
pushToBranchContext :: (Monad m)
                    => (EvalState node rezPath, Int, Int)
                    -> WebContextT node rezPath m ()
pushToBranchContext cont = WebContextT $ \c ->
    return ((), c{ branchContext = cont : branchContext c })

-- | Retrieve the frame on the top of the stack.
-- for more information regarding frames see 'pushToBranchContext'
popBranchContext :: (Monad m)
                 => WebContextT node rezPath m (EvalState node rezPath, Int, Int)
popBranchContext = WebContextT $ \c ->
    case branchContext c of
      [] -> error "popBranchContext - empty branch context"
      (x:xs) -> return (x, c{ branchContext = xs })

-- | Add seen node count and valid node count to the current
-- frame.
--
-- for more information regarding frames see 'pushToBranchContext'
addToBranchContext :: (Monad m)
                   => Int -> Int -> WebContextT node rezPath m ()
addToBranchContext count validCount = WebContextT $ \c ->
    case branchContext c of
      [] -> error "addToBranchContext - empty context stack"
      ((e,co,vc):xs) -> return ((), c{ branchContext = (e,co + count
                                                      ,vc + validCount): xs})

--------------------------------------------------
----            Unique bucket
--------------------------------------------------

-- | Initialisation function which must be called before the
-- beginning of a webrexp execution.
--
-- Inform the monad of the number of 'Unique' bucket in the
-- expression, permitting the allocation of the required number
-- of Set to hold them.
setBucketCount :: (Monad m, MonadIO m)
               => Int -- ^ Unique bucket count
               -> Int -- ^ Range counter count
               -> WebContextT node rezPath m ()
setBucketCount uniquecount rangeCount = WebContextT $ \c -> do
    arr <- liftIO $ newArray (0, uniquecount - 1) Set.empty
    counter <- liftIO $ newArray (0, rangeCount - 1) 0
    return ((), c{ uniqueBucket = arr
                 , countBucket = counter })

-- | Used for node range, return the current value of the
-- counter and increment it.
incrementGetRangeCounter :: (Monad m, MonadIO m)
                         => Int -> WebContextT node rezPath m Int
incrementGetRangeCounter bucket = WebContextT $ \c -> do
    num <- liftIO $ countBucket c `readArray`bucket
    liftIO . (countBucket c `writeArray` bucket) $ num + 1
    return (num, c)

-- | Tell if a string has already been recorded for a bucket ID.
-- Used for the implementation of the 'Unique' constructor of a webrexp.
--
-- Return False, unless 'setResourceVisited' has been called with the same
-- string before.
hasResourceBeenVisited :: (Monad m, MonadIO m)
                       => Int -> String -> WebContextT node rezPath m Bool
hasResourceBeenVisited bucketId str = WebContextT $ \c -> do
    set <- liftIO $ uniqueBucket c `readArray`bucketId
    return (str `Set.member` set, c)

-- | Record the visit of a string. 'hasResourceBeenVisited' will return True
-- for the same string after this call.
setResourceVisited :: (Monad m, MonadIO m)
                   => Int -> String -> WebContextT node rezPath m ()
setResourceVisited bucketId str = WebContextT $ \c -> do
    set <- liftIO $ uniqueBucket c `readArray` bucketId
    let newSet = str `Set.insert` set
    liftIO $ (uniqueBucket c `writeArray`bucketId) newSet
    return ((), c)

