{-# LANGUAGE FlexibleInstances #-}
module Text.Webrexp.IOMock ( IOMockable( .. ) ) where

import Control.Monad.ST
import Control.Applicative( (<$>) )

-- | Mocking IO, to let the software use IO actions normally,
-- and get back Nothing in case of non-authorization (like if
-- we want the code to stay in ST to be embeddable). Effectively
-- implement a form of sandboxing.
class IOMockable m where
    performIO :: IO a -> m (Maybe a)

instance IOMockable IO where
    performIO a = Just <$> a

instance IOMockable (ST s) where
    performIO _ = return Nothing

