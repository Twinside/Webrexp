module Webrexp.WebContext where

import Control.Applicative

data Context = Context
    { executionRoot :: String
    }

newtype WebContext a =
    WebContext { runWebContext :: Context -> (Context, a) }

instance Functor WebContext where
    {-# INLINE fmap #-}
    fmap f (WebContext a) =
        WebContext $ \c -> 
            let (c', a') = a c
            in (c', f a')

instance Applicative WebContext where
    {-# INLINE pure #-}
    pure a = WebContext $ \c -> (c, a)

    {-# INLINE (<*>) #-}
    (WebContext f) <*> (WebContext a) =
        WebContext $ \c ->
            let (c', a') = a c
                (c'', f') = f c'
            in (c'', f' a')

instance Monad WebContext where
    {-# INLINE return #-}
    return a = WebContext $ \c -> (c, a)

    {-# INLINE (>>=) #-}
    (WebContext m) >>= f =
        WebContext $ \c ->
            let (c', a) = m c
            in runWebContext (f a) c'

