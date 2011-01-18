{-# LANGUAGE ScopedTypeVariables #-}
module Webrexp.WebContext
    ( WebCrawler
    , WebContextT ( .. )
    , WebContext
    , NodeContext (..)
    , mapCurrentNodes 
    , evalWithEmptyContext
    , hasNodeLeft 
    , EvalState (..)

    , pushCurrentState 
    , popCurrentState 
    , setEvalState 
    )
    where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Functor.Identity
import Data.Maybe

import Control.Applicative
import Control.Monad

import Webrexp.GraphWalker

type WebCrawler node a = WebContextT node IO a
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
    , this :: node }

-- | This type represent the temporary results
-- of the evaluation of regexp.
data EvalState node =
      Nodes   [NodeContext node]
    | Strings [String]
    | Blob    [String]
    | None


data Context node = Context
    { executionRoot :: String,
      currentNodes :: EvalState node,
      contextStack :: [EvalState node]
    }

emptyContext :: Context node
emptyContext = Context
    { executionRoot = ""
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

