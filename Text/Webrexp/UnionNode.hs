{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- Here be dragons.
{-# LANGUAGE UndecidableInstances #-}

-- | This module has for aim to create new node type by combining
-- different GraphWalkers. The idea is to be able to walk from an
-- XML file to a Json file and so forth.
module Text.Webrexp.UnionNode( PartialGraph( .. ), UnionNode ( .. ) ) where

import Control.Applicative
import Control.Monad.IO.Class
import Network.HTTP
import System.Directory

import Text.Webrexp.GraphWalker
import Text.Webrexp.Remote.MimeTypes
import Text.Webrexp.ResourcePath
import qualified Text.Webrexp.ProjectByteString as B

-- | Extension of GraphWalker class to be able to query the type
-- about it's possibility of parsing. Very ad-hoc.
class (GraphWalker a rezPath) => PartialGraph a rezPath where
    -- | Tell if a node type can parse a given document, used
    -- in the node type decision. The first argument has to be
    -- ignored, so you can pass 'undefined' to it.
    isResourceParseable :: a -> rezPath -> ParseableType -> Bool

    -- | The real parsing function.
    -- The IO monad is only here to provide a way to log information
    -- TODO : find a better way.
    parseResource :: (MonadIO m)
                  => Loggers m -> rezPath -> ParseableType -> B.ByteString 
                  -> m (Maybe a)

-- | Data type which is an instance of graphwalker.
-- Use it to combine two other node types.
data UnionNode a b = UnionLeft a | UnionRight b
        deriving Eq

-- | Allow recursion of union node, so a tree of multidomain
-- node can be built.
instance ( PartialGraph a rezPath
         , PartialGraph b rezPath
         , GraphWalker (UnionNode a b) rezPath)
      => PartialGraph (UnionNode a b) rezPath where
    isResourceParseable _ datapath parser =
        isResourceParseable (undefined :: a) datapath parser ||
            isResourceParseable (undefined :: b) datapath parser

    parseResource loggers datapath parser binData =
        case ( isResourceParseable (undefined :: a) datapath parser
             , isResourceParseable (undefined :: b) datapath parser) of
            (True, _) -> parseResource loggers datapath parser binData >>= (\a -> return $ UnionLeft <$> a)
            (_   , _) -> parseResource loggers datapath parser binData >>= (\a -> return $ UnionRight <$> a)

instance (PartialGraph a ResourcePath, PartialGraph b ResourcePath)
        => GraphWalker (UnionNode a b) ResourcePath where

    attribOf att (UnionLeft a) = attribOf att a
    attribOf att (UnionRight a) = attribOf att a

    nameOf (UnionLeft a) = nameOf a
    nameOf (UnionRight a) = nameOf a

    childrenOf (UnionLeft a) =
        childrenOf a >>= \c -> return $ UnionLeft <$> c
    childrenOf (UnionRight a) =
        childrenOf a >>= \c -> return $ UnionRight <$> c

    valueOf (UnionLeft a) = valueOf a
    valueOf (UnionRight a) = valueOf a

    indirectLinks (UnionLeft a) = indirectLinks a
    indirectLinks (UnionRight a) = indirectLinks a

    accessGraph = loadData

    isHistoryMutable (UnionLeft a) = isHistoryMutable a
    isHistoryMutable (UnionRight a) = isHistoryMutable a

    deepValueOf (UnionLeft a) = deepValueOf a
    deepValueOf (UnionRight a) = deepValueOf a

parseUnion :: forall a b m.
              ( MonadIO m, Functor m
              , PartialGraph a ResourcePath
              , PartialGraph b ResourcePath )
           => Loggers m -> Maybe ParseableType -> ResourcePath -> B.ByteString
           -> m (AccessResult (UnionNode a b) ResourcePath)
parseUnion _ Nothing datapath binaryData =
    return $ DataBlob datapath binaryData

parseUnion loggers (Just parser) datapath binaryData =
    let binaryContent = DataBlob datapath binaryData
    in case ( isResourceParseable (undefined :: a) datapath parser
            , isResourceParseable (undefined :: b) datapath parser ) of
         (True,    _) ->
            maybe binaryContent 
                  (Result datapath . UnionLeft) <$> parseResource loggers datapath parser binaryData

         (   _, True) -> maybe binaryContent
                               (Result datapath . UnionRight)
                               <$> parseResource loggers datapath parser binaryData
         _            -> return binaryContent



loadData :: ( MonadIO m, Functor m
            , PartialGraph a ResourcePath
            , PartialGraph b ResourcePath )
         => Loggers m -> ResourcePath
         -> m (AccessResult (UnionNode a b) ResourcePath)
loadData loggers@(logger, _errLog, verbose) datapath@(Local s) = do
    logger $ "Opening file : '" ++ s ++ "'"
    realFile <- liftIO $ doesFileExist s
    if not realFile
       then do
           verbose $ "Unable to open file : " ++ s
           return AccessError
       else do file <- liftIO $ B.readFile s
       	       let kind = getParseKind s
       	       verbose $ "Found kind " ++ show kind ++ " for (" ++ s ++ ")"
       	       parseUnion loggers kind datapath file

loadData loggers@(logger, _, verbose) (Remote uri) = do
  logger $ "Downloading URL : '" ++ show uri ++ "'"
  (u, rsp) <- downloadBinary loggers uri
  let contentType = retrieveHeaders HdrContentType rsp
      binaryData = rspBody rsp
  case contentType of
    [] -> return $ DataBlob (Remote u) binaryData
    (hdr:_) -> do
        verbose $ "Downloaded (" ++ show u ++ ") ["
                                 ++ hdrValue hdr ++ "] "
        parseUnion loggers
                   (getParserForMimeType $ hdrValue hdr)
                   (Remote u) binaryData

