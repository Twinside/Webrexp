{-# LANGUAGE ScopedTypeVariables #-}
module Webrexp.WebContext
    ( 
    -- * Types
      WebCrawler
    , WebContextT ( .. )
    , WebContext
    , NodeContext (..)

    -- * User function
    , evalWithEmptyContext

    
    -- * Implementation info
    -- ** Evaluation function
    , mapCurrentNodes 
    , hasNodeLeft 
    , EvalState (..)

    -- ** State manipulation functions
    , pushCurrentState 
    , popCurrentState 
    , getEvalState 
    , setEvalState 

    -- ** Logging
    , textOutput 
    )
    where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Maybe

import Webrexp.ResourcePath
import Webrexp.GraphWalker

-- | Typical use of the WebContextT monad transformer
-- allowing to download information
type WebCrawler node a = WebContextT node IO a

-- | WebContext is 'WebContextT' as a simple Monad
type WebContext node a = WebContextT node Identity a

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
    | Blob    [String]
    | None


data Context node = Context
    { executionRoot :: ResourcePath,
      currentNodes :: EvalState node,
      contextStack :: [EvalState node]
    }

emptyContext :: Context node
emptyContext = Context
    { executionRoot = Local ""
    , contextStack = []
    , currentNodes = None }

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

textOutput :: String -> WebCrawler node ()
textOutput = liftIO . putStrLn
