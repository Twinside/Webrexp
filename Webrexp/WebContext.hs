{-# LANGUAGE ScopedTypeVariables #-}
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
    , getHttpDelay 
    , setHttpDelay 
    , isVerbose

    -- * User function
    , evalWithEmptyContext
    
    -- * Implementation info
    -- ** Evaluation function
    , hasNodeLeft 
    , prepareLogger 

    -- ** State manipulation functions
    , pushCurrentState 
    , popCurrentState 
    , getEvalState 
    , setEvalState 
    , dumpCurrentState

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
import Data.Array
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
    { parents :: [(node, Int)]
    , this :: node 
    , rootRef :: rezPath
    }

data BinBlob rezPath = BinBlob
    { sourcePath :: rezPath
    , blobData :: B.ByteString
    }

-- | This type represent the temporary results
-- of the evaluation of regexp.
data EvalState node rezPath =
      Node (NodeContext node rezPath)
    | Text String
    | Blob (BinBlob rezPath)

data LogLevel = Quiet | Normal | Verbose deriving (Eq)

data Context node rezPath = Context
    { currentNodes :: [EvalState node rezPath]
    , contextStack :: [[EvalState node rezPath]]
    , logLevel :: LogLevel
    , httpDelay :: Int
    , httpUserAgent :: String
    , defaultOutput :: Handle
    , uniqueBucket :: Array Int (Set.Set String) 
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
    , currentNodes = []
    , logLevel = Normal
    , httpDelay = 1500
    , httpUserAgent = ""
    , defaultOutput = stdout
    , uniqueBucket = array (0,0) []
    }

--------------------------------------------------
----            Getter/Setter
--------------------------------------------------
setHttpDelay :: (Monad m) => Int -> WebContextT node rezPath m ()
setHttpDelay delay = WebContextT $ \c ->
        return ((), c{ httpDelay = delay })

getHttpDelay :: (Monad m) => WebContextT node rezPath m Int
getHttpDelay = WebContextT $ \c -> return (httpDelay c, c)

setOutput :: (Monad m) => Handle -> WebContextT node rezPath m ()
setOutput handle = WebContextT $ \c ->
        return ((), c{ defaultOutput = handle })

setUserAgent :: (Monad m) => String -> WebContextT node rezPath m ()
setUserAgent usr = WebContextT $ \c ->
    return ((), c{ httpUserAgent = usr })

getUserAgent :: (Monad m) => WebContextT node rezPath m String
getUserAgent = WebContextT $ \c -> return (httpUserAgent c, c)

setLogLevel :: (Monad m) => LogLevel -> WebContextT node rezPath m ()
setLogLevel lvl = WebContextT $ \c ->
    return ((), c{logLevel = lvl})

isVerbose :: (Monad m) => WebContextT node rezPath m Bool
isVerbose = WebContextT $ \c -> 
    return (logLevel c == Verbose, c)

pushCurrentState :: (Monad m) => WebContextT node rezPath m ()
pushCurrentState = WebContextT $ \c ->
        return ((), c{ contextStack = 
                        currentNodes c : contextStack c })

popCurrentState :: (Monad m) => WebContextT node rezPath m ()
popCurrentState = WebContextT $ \c ->
    case contextStack c of
         []     -> error "Empty context stack, implementation bug"
         (x:xs) -> 
            return ((), c{ contextStack = xs, currentNodes = x })

evalWithEmptyContext :: (Monad m)
                     => WebContextT node rezPath m a -> m a
evalWithEmptyContext val = do
    (finalVal, _context) <- runWebContextT val emptyContext
    return finalVal

hasNodeLeft :: (Monad m) => WebContextT node rezPath m Bool
hasNodeLeft = WebContextT $ \c -> return (not . null $ currentNodes c, c)

-- | Allow the interpreter to change the evaluation state of
-- monad.
setEvalState :: (Monad m)
             => [EvalState node rezPath] -> WebContextT node rezPath m ()
setEvalState st = WebContextT $ \c ->
    return ((), c { currentNodes = st })

getEvalState :: (Monad m) => WebContextT node rezPath m [EvalState node rezPath]
getEvalState = WebContextT $ \c ->
    return (currentNodes c, c)

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

dumpCurrentState :: (GraphWalker node rezPath)
                 => WebContextT node rezPath IO ()
dumpCurrentState = WebContextT $ \c -> do
    mapM_ (\e -> case e of
      Blob b -> putStrLn $ "Blob (" ++ (show $ sourcePath b) ++ ")"
      Node n -> putStrLn $ show (this n)
      Text s -> putStrLn s) (currentNodes c) >> return ((), c)

--------------------------------------------------
----            Unique bucket
--------------------------------------------------
setUniqueBucketCount :: (Monad m) => Int -> WebContextT node rezPath m ()
setUniqueBucketCount count = WebContextT $ \c ->
    return ((), c{ uniqueBucket = listArray (0, count - 1) $ repeat Set.empty})

hasResourceBeenVisited :: (Monad m) => Int -> String -> WebContextT node rezPath m Bool
hasResourceBeenVisited bucketId str = WebContextT $ \c ->
    return (str `Set.member` (uniqueBucket c ! bucketId), c)

setResourceVisited :: (Monad m) => Int -> String -> WebContextT node rezPath m ()
setResourceVisited bucketId str = WebContextT $ \c ->
    let buckets = uniqueBucket c
        newSet = Set.insert str $ buckets ! bucketId
    in return ((), c{ uniqueBucket = buckets // [(bucketId, newSet)]  })
      
