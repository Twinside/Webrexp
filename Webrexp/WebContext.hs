{-# LANGUAGE ScopedTypeVariables #-}
module Webrexp.WebContext
    ( 
    -- * Types
      WebCrawler
    , WebContextT ( .. )
    , WebContext 
    , NodeContext (..)
    , EvalState (..)
    , Context

    -- * Crawling configuration
    , LogLevel (..)
    , setLogLevel 
    , getUserAgent 
    , setUserAgent 
    , setOutput 
    , getHttpDelay 
    , setHttpDelay 
    , isVerbose

    -- * User function
    , evalWithEmptyContext
    
    -- * Implementation info
    -- ** Evaluation function
    , mapCurrentNodes 
    , hasNodeLeft 
    , prepareLogger 

    -- ** State manipulation functions
    , pushCurrentState 
    , popCurrentState 
    , getEvalState 
    , setEvalState 

    -- ** Unicity manipulation function
    , setUniqueBucketCount 
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
import Data.Maybe
import Data.Array
import qualified Data.Set as Set

import Webrexp.ResourcePath
import Webrexp.GraphWalker

-- | Typical use of the WebContextT monad transformer
-- allowing to download information
type WebCrawler node a = WebContextT node IO a

-- | WebContext is 'WebContextT' as a simple Monad
type WebContext node a = WebContextT node Identity a

-- | Represent a graph node and the path
-- used to go up to it.
data NodeContext node = NodeContext
    { parents :: [(node, Int)]
    , this :: node 
    , rootRef :: ResourcePath
    }

-- | This type represent the temporary results
-- of the evaluation of regexp.
data EvalState node =
      Nodes   [NodeContext node]
    | Strings [String]
    | None

data LogLevel = Quiet | Normal | Verbose deriving (Eq)

data Context node = Context
    { currentNodes :: EvalState node
    , contextStack :: [EvalState node]
    , logLevel :: LogLevel
    , httpDelay :: Int
    , httpUserAgent :: String
    , defaultOutput :: Handle
    , uniqueBucket :: Array Int (Set.Set String) 
    }

--------------------------------------------------
----            Monad definitions
--------------------------------------------------
newtype (Monad m) => WebContextT node m a =
    WebContextT { runWebContextT :: Context node
                                 -> m (a, Context node) }

instance (Functor m, Monad m) => Functor (WebContextT node m) where
    {-# INLINE fmap #-}
    fmap f a = WebContextT $ \c ->
        fmap (\(a', c') -> (f a', c')) $ runWebContextT a c

instance (Functor m, Monad m) => Applicative (WebContextT node m) where
    pure = return
    (<*>) = ap

instance (Monad m) => Monad (WebContextT node m) where
    {-# INLINE return #-}
    return a =
        WebContextT $ \c -> return (a, c)

    {-# INLINE (>>=) #-}
    (WebContextT val) >>= f = WebContextT $ \c -> do
        (val', c') <- val c
        runWebContextT (f val') c'

instance MonadTrans (WebContextT node) where
    lift m = WebContextT $ \c -> do
        a <- m
        return (a, c)

instance (MonadIO m) => MonadIO (WebContextT node m) where
    liftIO = lift . liftIO

--------------------------------------------------
----            Context manipulation
--------------------------------------------------

emptyContext :: Context node
emptyContext = Context
    { contextStack = []
    , currentNodes = None
    , logLevel = Normal
    , httpDelay = 1500
    , httpUserAgent = ""
    , defaultOutput = stdout
    , uniqueBucket = array (0,0) []
    }

--------------------------------------------------
----            Getter/Setter
--------------------------------------------------
setHttpDelay :: (Monad m) => Int -> WebContextT node m ()
setHttpDelay delay = WebContextT $ \c ->
        return ((), c{ httpDelay = delay })

getHttpDelay :: (Monad m) => WebContextT node m Int
getHttpDelay = WebContextT $ \c -> return (httpDelay c, c)

setOutput :: (Monad m) => Handle -> WebContextT node m ()
setOutput handle = WebContextT $ \c ->
        return ((), c{ defaultOutput = handle })

setUserAgent :: (Monad m) => String -> WebContextT node m ()
setUserAgent usr = WebContextT $ \c ->
    return ((), c{ httpUserAgent = usr })

getUserAgent :: (Monad m) => WebContextT node m String
getUserAgent = WebContextT $ \c -> return (httpUserAgent c, c)

setLogLevel :: (Monad m) => LogLevel -> WebContextT node m ()
setLogLevel lvl = WebContextT $ \c ->
    return ((), c{logLevel = lvl})

isVerbose :: (Monad m) => WebContextT node m Bool
isVerbose = WebContextT $ \c -> 
    return (logLevel c == Verbose, c)

pushCurrentState :: (Monad m) => WebContextT node m ()
pushCurrentState = WebContextT $ \c ->
        return ((), c{ contextStack = 
                        currentNodes c : contextStack c })

popCurrentState :: (Monad m) => WebContextT node m ()
popCurrentState = WebContextT $ \c ->
    case contextStack c of
         []     -> error "Empty context stack, implementation bug"
         (x:xs) -> 
            return ((), c{ contextStack = xs, currentNodes = x })

evalWithEmptyContext :: (Monad m)
                     => WebContextT node m a -> m a
evalWithEmptyContext val = do
    (finalVal, _context) <- runWebContextT val emptyContext
    return finalVal

hasNodeLeft :: (Monad m) => WebContextT node m Bool
hasNodeLeft = WebContextT $ \c ->
    case currentNodes c of
      Nodes n -> return (not $ null n, c)
      _       -> return (False, c)

-- | Allow the interpreter to change the evaluation state of
-- monad.
setEvalState :: (Monad m)
             => EvalState node -> WebContextT node m ()
setEvalState st = WebContextT $ \c ->
    return ((), c { currentNodes = st })

getEvalState :: (Monad m) => WebContextT node m (EvalState node)
getEvalState = WebContextT $ \c ->
    return (currentNodes c, c)

-- | Map operation performing it only if the evaluation
-- state is a list of nodes.
mapCurrentNodes :: (Monad m, GraphWalker node)
                => (NodeContext node -> Maybe (NodeContext node))
                -> WebContextT node m ()
mapCurrentNodes f = WebContextT $ \c ->
    case currentNodes c of
      Nodes nodes ->
         let newNodes = catMaybes $ map f nodes
         in return ((), c{ currentNodes = Nodes newNodes })
      _ -> return ((), c)

-- | Return normal, error, verbose logger
prepareLogger :: (Monad m)
              => WebContextT node m (Logger, Logger, Logger)
prepareLogger = WebContextT $ \c ->
    let silenceLog = \_ -> return ()
        errLog = hPutStrLn stderr
        normalLog = hPutStrLn stdout
    in case logLevel c of
      Quiet -> return ((silenceLog, errLog, silenceLog), c)
      Normal -> return ((normalLog, errLog, silenceLog), c)
      Verbose -> return ((normalLog, errLog, normalLog), c)

--------------------------------------------------
----            Unique bucket
--------------------------------------------------
setUniqueBucketCount :: (Monad m) => Int -> WebContextT node m ()
setUniqueBucketCount count = WebContextT $ \c ->
    return ((), c{ uniqueBucket = listArray (0, count - 1) $ repeat Set.empty})

hasResourceBeenVisited :: (Monad m) => Int -> String -> WebContextT node m Bool
hasResourceBeenVisited bucketId str = WebContextT $ \c ->
    return (str `Set.member` (uniqueBucket c ! bucketId), c)

setResourceVisited :: (Monad m) => Int -> String -> WebContextT node m ()
setResourceVisited bucketId str = WebContextT $ \c ->
    let buckets = uniqueBucket c
        newSet = Set.insert str $ buckets ! bucketId
    in return ((), c{ uniqueBucket = buckets // [(bucketId, newSet)]  })
      
