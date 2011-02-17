{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
-- Here lies dragons.
{-# LANGUAGE UndecidableInstances #-}

-- | This module has for aim to create new node type by combining
-- different GraphWalkers. The idea is to be able to walk from an
-- XML file to a Json file and so forth.
module Webrexp.UnionNode( PartialGraph( .. ), UnionNode ( .. ) ) where

import Control.Applicative
import Control.Monad.IO.Class
import Network.HTTP
import System.Directory

import Webrexp.GraphWalker
import Webrexp.Remote.MimeTypes
import Webrexp.ResourcePath
import qualified Webrexp.ProjectByteString as B

-- | Extension of GraphWalker class to be able to query the type
-- about it's possibility of parsing. Very ad-hoc.
class (GraphWalker a rezPath) => PartialGraph a rezPath where
    -- | Provide a dummy element just to be passed at 'isResourceParseable'.
    -- Forcing a monoid instance was not ideal, so here is the hack.
    dummyElem :: a

    -- | Tell if a node type can parse a given document, used
    -- in the node type decision.
    isResourceParseable :: a -> ParseableType -> Bool

    -- | The real parsing function.
    parseResource :: ParseableType -> B.ByteString -> Maybe a

-- | Data type which is an instance of graphwalker.
-- Use it to combine two other node types.
data UnionNode a b = UnionLeft a | UnionRight b

instance (PartialGraph a ResourcePath, PartialGraph b ResourcePath)
        => GraphWalker (UnionNode a b) ResourcePath where

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

    accessGraph = loadData

parseUnion :: forall a b m.
              ( MonadIO m
              , PartialGraph a ResourcePath
              , PartialGraph b ResourcePath )
           => Maybe ParseableType -> ResourcePath -> B.ByteString
           -> m (AccessResult (UnionNode a b) ResourcePath)
parseUnion Nothing datapath binaryData =
    return $ DataBlob datapath binaryData

parseUnion (Just parser) datapath binaryData =
    let binaryContent = DataBlob datapath binaryData
    in case ( isResourceParseable (dummyElem :: a) parser
            , isResourceParseable (dummyElem :: b) parser ) of
         (True,    _) -> maybe (return binaryContent)
                               (return . Result datapath . UnionLeft) 
                               $ parseResource parser binaryData
         (   _, True) -> maybe (return binaryContent)
                               (return . Result datapath . UnionRight)
                               $ parseResource parser binaryData
         _            -> return binaryContent



loadData :: ( MonadIO m
            , PartialGraph a ResourcePath
            , PartialGraph b ResourcePath )
         => Loggers -> ResourcePath
         -> m (AccessResult (UnionNode a b) ResourcePath)
loadData (logger, _errLog, _verbose) datapath@(Local s) = do
    liftIO . logger $ "Opening file : '" ++ s ++ "'"
    realFile <- liftIO $ doesFileExist s
    if not realFile
       then return AccessError
       else do file <- liftIO $ B.readFile s
       	       parseUnion (getParseKind s) datapath file

loadData loggers@(logger, _, verbose) (Remote uri) = do
  liftIO . logger $ "Downloading URL : '" ++ show uri ++ "'"
  (u, rsp) <- downloadBinary loggers uri
  let contentType = retrieveHeaders HdrContentType rsp
      binaryData = rspBody rsp
  case contentType of
    [] -> return $ DataBlob (Remote u) binaryData
    (hdr:_) -> do
        liftIO . verbose $ "Downloaded (" ++ show u ++ ") ["
                                      ++ hdrValue hdr ++ "] "
        parseUnion (getParserForMimeType $ hdrValue hdr)
                   (Remote u) binaryData

