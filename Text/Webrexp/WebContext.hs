{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- | This module define the state carryied during the webrexp
-- evaluation. This state is implemented as a monad transformer
-- on top of 'IO'.
module Text.Webrexp.WebContext
    ( 
    -- * Types
      WebCrawler
    , WebContextT ( .. )
    , WebContext 
    , NodeContext (..)
    , EvalState (..)
    , BinBlob (..)
    , Context
    , HistoryPath (..)

    -- * Aliases
    -- Only used to provide more meaningful type signatures
    , Counter
    , SeenCounter
    , ValidSeenCounter
    , StateNumber

    -- * Node manipulation function/operators
    , (^:)
    , (^+)

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
    , executeWithEmptyContext 
    , repurposeNode 
    
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
    , accumulateCurrentState 
    , popAccumulation 

    , pushToBranchContext 
    , popBranchContext 
    , addToBranchContext 

    -- ** Unicity manipulation function
    , setBucketCount 
    , incrementGetRangeCounter 
    , hasResourceBeenVisited
    , setResourceVisited

    -- * Log system
    , debugLog
    , textOutput
    )
    where

import System.IO
import Control.Applicative
import Control.Arrow( first )
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Array.MArray
import qualified Data.Set as Set
import Control.Exception( IOException )
import qualified Control.Exception as Ex

import qualified Text.Webrexp.ProjectByteString as B
import Text.Webrexp.GraphWalker
import Text.Webrexp.IOMock

-- | Typical use of the WebContextT monad transformer
-- allowing to download information
type WebCrawler array node rezPath a = WebContextT array node rezPath IO a

-- | WebContext is 'WebContextT' as a simple Monad
type WebContext array node rezPath a = WebContextT array node rezPath Identity a

-- | Record a graph path in a document, from the last indirect
-- node to this one.
data HistoryPath node =
      -- | A path in an immutable graph. The graph that
      -- doesn't move under our feets, so we store the
      -- index of the followgin node in the path.
      ImmutableHistory [(node, Int)]

      -- | If the graph is suceptible to move under our
      -- feets, we have to search again for the position
      -- of the node in the parent node.
    | MutableHistory   [node]

-- | Fuse two history together, is equivalent to the '++'
-- operator for list.
(^+) :: [(node, Int)] -> HistoryPath node -> HistoryPath node
(^+) nodes (MutableHistory hist) = MutableHistory $ map fst nodes ++ hist
(^+) nodes (ImmutableHistory hist) = ImmutableHistory $ nodes ++ hist

-- | Append at info at the beginning of an history,
-- equivalent to the ':' operator for lists.
(^:) :: (node, Int) -> HistoryPath node -> HistoryPath node
(^:) (node, _) (MutableHistory hist) = MutableHistory $ node : hist
(^:) node (ImmutableHistory hist) = ImmutableHistory $ node : hist

-- | Represent a graph node and the path
-- used to go up to it.
data NodeContext node rezPath = NodeContext
    { -- | Path from the root of the document to
      -- 'this' node.
      parents :: HistoryPath node

      -- | Real node value
    , this :: node              

      -- | The last indirect path used to get to this node.
    , rootRef :: rezPath       
    }

-- | Function useful if used in combination of an union-node :
-- - A function produce a node context for a specific type
-- - You want to generalise it for a complex union
-- - Use this function :)
--
-- For example to produce a simple union node :
--
-- > repurposeNode UnionRight $ initialSimpleNode
repurposeNode :: (nodeA -> nodeB) -> NodeContext nodeA rezPath
              -> NodeContext nodeB rezPath
repurposeNode f node = NodeContext
    { parents = historyPatcher $ parents node
    , this = f $ this node
    , rootRef = rootRef node
    }
  where historyPatcher (ImmutableHistory hist) =
            ImmutableHistory $ map (first f) hist
        historyPatcher (MutableHistory hist) =
            MutableHistory $ map f hist

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

-- | An int used as a counter
type Counter = Int

-- | Number of elements seen at a state in the automata.
type SeenCounter = Int

-- | Number of elements which arrived by a true transition
-- to a state in the automata.
type ValidSeenCounter = Int

-- | Just an index to a state in the automata.
type StateNumber = Int

-- | Internal data context.
data Context array node rezPath = Context
    { -- | Context stack used in breadth-first evaluation
      contextStack :: [([EvalState node rezPath], Counter)]

      -- | State waiting to be executed in a depth-
      -- first execution.
    , waitingStates :: [(EvalState node rezPath, StateNumber)]

      -- | State used to implement branches in the depth
      -- first evaluator.
    , branchContext :: [(EvalState node rezPath, SeenCounter, ValidSeenCounter)]

      -- | Buckets used for uniqueness pruning, all
      -- evaluation kind.
    , uniqueBucket :: array Int (Set.Set String)

      -- | Counters used for range evaluation in DFS
    , countBucket :: array Int Counter

      -- | Tell if we must keep the found information in memory
      -- instead of directly dumping it on screen.
    , mustGatherData :: Bool

      -- | If you want to run a webrexp in library mode
    , gatheredData :: [Either String String]

      -- | Current log level
    , logLevel :: LogLevel
    , httpDelay :: Int
    , httpUserAgent :: String
    , defaultOutput :: Handle
    }

--------------------------------------------------
----            Monad definitions
--------------------------------------------------
newtype (Monad m) => WebContextT array node rezPath m a =
    WebContextT { runWebContextT :: Context array node rezPath
                                 -> m (a, Context array node rezPath) }

instance (Functor m, Monad m) => Functor (WebContextT array node rezPath m) where
    {-# INLINE fmap #-}
    fmap f a = WebContextT $ \c ->
        fmap (first f) $ runWebContextT a c

instance (Functor m, Monad m) => Applicative (WebContextT array node rezPath m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Monad (WebContextT array node rezPath m) where
    {-# INLINE return #-}
    return a =
        WebContextT $ \c -> return (a, c)

    {-# INLINE (>>=) #-}
    (WebContextT val) >>= f = WebContextT $ \c -> do
        (val', c') <- val c
        runWebContextT (f val') c'

instance MonadTrans (WebContextT array node rezPath) where
    lift m = WebContextT $ \c -> do
        a <- m
        return (a, c)

instance (MonadIO m) => MonadIO (WebContextT array node rezPath m) where
    liftIO = lift . liftIO

instance IOMockable (WebContextT array node rezPath IO) where
    performIO act = Just <$> liftIO act

--------------------------------------------------
----            Context manipulation
--------------------------------------------------

emptyContext :: Context array node rezPath
emptyContext = Context
    { contextStack = []
    , waitingStates = []
    , branchContext = []
    , logLevel = Normal
    , httpDelay = 1500
    , httpUserAgent = ""
    , mustGatherData = False
    , gatheredData = []
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
setHttpDelay :: (Monad m) => Int -> WebContextT array node rezPath m ()
setHttpDelay delay = WebContextT $ \c ->
        return ((), c{ httpDelay = delay })

-- | return the value set by 'setHttpDelay'
getHttpDelay :: (Monad m) => WebContextT array node rezPath m Int
getHttpDelay = WebContextT $ \c -> return (httpDelay c, c)

-- | Define the text output for written text.
setOutput :: (Monad m) => Handle -> WebContextT array node rezPath m ()
setOutput handle = WebContextT $ \c ->
        return ((), c{ defaultOutput = handle })

-- | Retrieve the default file output used for text.
getOutput :: (Monad m) => WebContextT array node rezPath m Handle
getOutput = WebContextT $ \c -> return (defaultOutput c, c)

isDataOutputedDirectly :: (Monad m) => WebContextT array node rezPath m Bool
isDataOutputedDirectly = WebContextT $ \c ->
        return (not $ mustGatherData c, c)

-- | Set the user agent which must be used for indirect operations
--
-- The value is stored but not used yet.
setUserAgent :: (Monad m) => String -> WebContextT array node rezPath m ()
setUserAgent usr = WebContextT $ \c ->
    return ((), c{ httpUserAgent = usr })

-- | return the value set by 'setUserAgent'
getUserAgent :: (Monad m) => WebContextT array node rezPath m String
getUserAgent = WebContextT $ \c -> return (httpUserAgent c, c)

-- | Set the value of the logging level.
setLogLevel :: (Monad m) => LogLevel -> WebContextT array node rezPath m ()
setLogLevel lvl = WebContextT $ \c ->
    return ((), c{logLevel = lvl})

-- | Tell if the current 'LoggingLevel' is set to 'Verbose'
isVerbose :: (Monad m) => WebContextT array node rezPath m Bool
isVerbose = WebContextT $ \c -> 
    return (logLevel c == Verbose, c)

-- | TODO : write documentation
accumulateCurrentState :: (Monad m)
                       => ([EvalState node rezPath], StateNumber)
                       -> WebContextT array node rezPath m ()
accumulateCurrentState lst = WebContextT $ \c ->
        return ((), c{ contextStack = lst : contextStack c })

-- | TODO : write documentation
popAccumulation :: (Monad m)
                => WebContextT array node rezPath m ([EvalState node rezPath], StateNumber)
popAccumulation = WebContextT $ \c ->
    case contextStack c of
         []     -> error "Empty context stack, implementation bug"
         (x:xs) -> return (x, c{ contextStack = xs })

-- | Internally the monad store a stack of state : the list
-- of currently evaluated 'EvalState'. Pushing this context
-- with store all the current nodes in it, waiting for later
-- retrieval.
pushCurrentState :: (Monad m)
                 => [EvalState node rezPath]
                 -> WebContextT array node rezPath m ()
pushCurrentState lst = WebContextT $ \c ->
        return ((), c{ contextStack = (lst, 0) : contextStack c })

-- | Inverse operation of 'pushCurrentState', retrieve
-- stored nodes.
popCurrentState :: (Monad m)
                => WebContextT array node rezPath m [EvalState node rezPath]
popCurrentState = WebContextT $ \c ->
    case contextStack c of
         []     -> error "Empty context stack, implementation bug"
         ((x,_):xs) -> 
            return (x, c{ contextStack = xs })

-- | Helper function used to start the evaluation of a webrexp
-- with a default context, with sane defaults.
evalWithEmptyContext :: (Monad m)
                     => WebContextT array node rezPath m a -> m a
evalWithEmptyContext val = do
    (finalVal, _context) <- runWebContextT val emptyContext
    return finalVal

-- | Helper function used to evaluate a webrexp and get back
-- data with a default context with sane defaults.
executeWithEmptyContext :: (Monad m)
                        => WebContextT array node rezPath m a -> m [Either String String]
executeWithEmptyContext val = do
    (_, context) <- runWebContextT val (emptyContext { mustGatherData = True })
    return $ gatheredData context

-- | Return normal, error, verbose logger
prepareLogger :: (Monad m, IOMockable (WebContextT array node rezPath m))
              => WebContextT array node rezPath m 
                    (Logger (WebContextT array node rezPath m)
                    ,Logger (WebContextT array node rezPath m)
                    ,Logger (WebContextT array node rezPath m))
prepareLogger = WebContextT $ \c ->
    let silenceLog _ = return ()
        errLog msg = performIO (hPutStrLn stderr msg) >> return ()
        normalLog = textOutput
    in case (mustGatherData c, logLevel c) of
      (True, _) -> return ((silenceLog, gatheringLog . Left, silenceLog), c)
      (_, Quiet) -> return ((silenceLog, errLog, silenceLog), c)
      (_, Normal) -> return ((normalLog, errLog, silenceLog), c)
      (_, Verbose) -> return ((normalLog, errLog, normalLog), c)

-- | Debugging function, only displayed in verbose
-- logging mode.
debugLog :: (IOMockable (WebContextT array node rezPath m), Monad m)
         => String -> WebContextT array node rezPath m ()
debugLog str = do
    verb <- isVerbose
    when verb (performIO (putStrLn str) >> return ())


-- | If a webrexp output some text, it must go through
-- this function. It ensure the writting in the correct
-- file.
textOutput :: (Monad m, IOMockable (WebContextT array node rezPath m)) 
           => String -> WebContextT array node rezPath m ()
textOutput str = do
    direct <- isDataOutputedDirectly 
    if not direct
       then gatheringLog $ Right str
       else do handle <- getOutput
               _ <- performIO $ Ex.catch (hPutStr handle str)
                       (\e -> hPutStrLn stderr $ "Writing error : " ++ 
                                                show (e :: IOException))
               return ()

-- | Keep track of an error or a normal log in the application monad
-- transformer.
gatheringLog :: (Monad m) => Either String String -> WebContextT array node rezPath m ()
gatheringLog d = WebContextT $ \c ->
    return ((), c { gatheredData = gatheredData c ++ [d] })


--------------------------------------------------
----            Depth First evaluation
--------------------------------------------------

-- | Record a node in the context for the DFS evaluation.
recordNode :: (Monad m) 
           => (EvalState node rezPath, StateNumber) -> WebContextT array node rezPath m ()
recordNode n = WebContextT $ \c ->
    return ((), c{ waitingStates = n : waitingStates c })

-- | Get the last record from the top of the stack
popLastRecord :: (Monad m)
              => WebContextT array node rezPath m (EvalState node rezPath, StateNumber)
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
                    => (EvalState node rezPath, SeenCounter, ValidSeenCounter)
                    -> WebContextT array node rezPath m ()
pushToBranchContext cont = WebContextT $ \c ->
    return ((), c{ branchContext = cont : branchContext c })

-- | Retrieve the frame on the top of the stack.
-- for more information regarding frames see 'pushToBranchContext'
popBranchContext :: (Monad m)
                 => WebContextT array node rezPath m ( EvalState node rezPath
                                                     , SeenCounter
                                                     , ValidSeenCounter )
popBranchContext = WebContextT $ \c ->
    case branchContext c of
      [] -> error "popBranchContext - empty branch context"
      (x:xs) -> return (x, c{ branchContext = xs })

-- | Add seen node count and valid node count to the current
-- frame.
--
-- for more information regarding frames see 'pushToBranchContext'
addToBranchContext :: (Monad m)
                   => SeenCounter -> ValidSeenCounter 
                   -> WebContextT array node rezPath m ()
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
setBucketCount :: ( Monad m
                  , MArray array Counter m
                  , MArray array (Set.Set String) m
                  )
               => Int -- ^ Unique bucket count
               -> Int -- ^ Range counter count
               -> WebContextT array node rezPath m ()
setBucketCount uniquecount rangeCount = WebContextT $ \c -> do
    setArray <- newArray (0, uniquecount - 1) Set.empty
    counterArray <- newArray (0, rangeCount - 1) 0
    return ((), c{ uniqueBucket = setArray
                 , countBucket = counterArray } )

-- | Used for node range, return the current value of the
-- counter and increment it.
incrementGetRangeCounter :: (Monad m, MArray array Counter m)
                         => Int -> WebContextT array node rezPath m Int
incrementGetRangeCounter bucket = WebContextT $ \c -> do
    num <- countBucket c `readArray` bucket
    writeArray (countBucket c) bucket $ num + 1
    return (num, c)

-- | Tell if a string has already been recorded for a bucket ID.
-- Used for the implementation of the 'Unique' constructor of a webrexp.
--
-- Return False, unless 'setResourceVisited' has been called with the same
-- string before.
hasResourceBeenVisited :: (Monad m, MArray array (Set.Set String) m)
                       => Int -> String -> WebContextT array node rezPath m Bool
hasResourceBeenVisited bucketId str = WebContextT $ \c -> do
    set <- uniqueBucket c `readArray` bucketId
    return (str `Set.member` set, c)

-- | Record the visit of a string. 'hasResourceBeenVisited' will return True
-- for the same string after this call.
setResourceVisited :: (Monad m, MArray array (Set.Set String) m)
                   => Int -> String -> WebContextT array node rezPath m ()
setResourceVisited bucketId str = WebContextT $ \c -> do
    set <- uniqueBucket c `readArray` bucketId
    let newSet = str `Set.insert` set
    writeArray (uniqueBucket c) bucketId newSet
    return ((), c)

