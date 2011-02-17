{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- Here lies dragons.
{-# LANGUAGE UndecidableInstances #-}
module Webrexp.UnionNode( PartialGraph( .. ), UnionNode ( .. ) ) where

import Control.Applicative
import Control.Monad.IO.Class
import Webrexp.GraphWalker
import Webrexp.Remote.MimeTypes
import qualified Webrexp.ProjectByteString as B

class (GraphWalker a rezPath) => PartialGraph a rezPath | a -> rezPath where
    dummyElem :: a
    isResourceParseable :: a -> ParseableType -> Bool
    parseResource :: rezPath -> B.ByteString -> a

data UnionNode a b = UnionLeft a | UnionRight b

instance (PartialGraph a rezPath, PartialGraph b rezPath)
        => GraphWalker (UnionNode a b) rezPath where

    attribOf att (UnionLeft a) = attribOf att a
    attribOf att (UnionRight a) = attribOf att a

    nameOf (UnionLeft a) = nameOf a
    nameOf (UnionRight a) = nameOf a

    childrenOf (UnionLeft a) = UnionLeft <$> childrenOf a
    childrenOf (UnionRight a) = UnionRight <$> childrenOf a

    valueOf (UnionLeft a) = valueOf a
    valueOf (UnionRight a) = valueOf a

    indirectLinks (UnionLeft a) = indirectLinks a
    indirectLinks (UnionRight a) = indirectLinks a

    accessGraph = unionAccess

unionAccess :: (MonadIO m, PartialGraph a rezPath, PartialGraph b rezPath)
            => Loggers -> rezPath -> m (AccessResult (UnionNode a b)  rezPath)
unionAccess _loggers _datapath = return AccessError

