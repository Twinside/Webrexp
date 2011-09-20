{-# LANGUAGE FlexibleInstances #-}
module Text.Webrexp.IOMock ( IOMockable( .. ) ) where

{-import Control.Monad.IO.Class-}
{-import Control.Monad.ST-}

class IOMockable m where
    performIO :: IO a -> m (Maybe a)

{-instance (MonadIO m) => IOMockable m where-}
    {-performIO io = liftIO io >>= return . Just-}

{-instance IOMockable (ST s) where-}
    {-performIO _ = return Nothing-}

