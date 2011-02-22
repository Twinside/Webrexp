{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Webrexp.HaXmlNode( HaXmLNode ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Maybe( catMaybes )
import Network.HTTP
import Network.URI
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Html.Parse
import System.Directory

import qualified Webrexp.ProjectByteString as B

import Webrexp.GraphWalker
import Webrexp.ResourcePath
import Webrexp.Remote.MimeTypes
import Webrexp.UnionNode

type HaXmLNode = Content Posn

instance PartialGraph HaXmLNode ResourcePath where
    dummyElem = undefined

    isResourceParseable _ _ ParseableHTML = True
    isResourceParseable _ _ ParseableXML = True
    isResourceParseable _ _ _ = False

    parseResource _ ParseableHTML bindata = Just $ CElem e noPos
        where (Document _prolog _ e _) = htmlParse "" $ B.unpack bindata
    parseResource _ ParseableXML bindata =
        case xmlParse' "" $ B.unpack bindata of
            Left _ -> Nothing
            Right (Document _prolog _ e _) -> Just $ CElem e noPos
    parseResource _ _ _ = error "Cannot parse"

instance GraphWalker HaXmLNode ResourcePath where
    accessGraph = loadHtml


    attribOf attrName (CElem (Elem _ attrList _) _) =
        show <$> lookup attrName attrList
    attribOf _ _ = Nothing

    childrenOf = return . pureChildren

    nameOf (CElem (Elem n _ _) _) = Just n
    nameOf _ = Nothing

    indirectLinks n =
        catMaybes [ attribOf "href" n >>= importPath
                  , attribOf "src" n >>= importPath ]

    isHistoryMutable _ = False

    valueOf (CString _ sdata _) = sdata
    valueOf a = case pureChildren a of
       (CString _ txt _:_) -> txt
       _ -> ""

pureChildren :: HaXmLNode -> [HaXmLNode]
pureChildren (CElem (Elem _ _ children) _) = children
pureChildren _ = []

parserOfKind :: Maybe ParseableType
             -> ResourcePath
             -> B.ByteString -> AccessResult HaXmLNode ResourcePath
parserOfKind Nothing datapath = DataBlob datapath
parserOfKind (Just ParseableHTML) datapath = \file ->
    let (Document _prolog _ e _) = htmlParse "" $ B.unpack file
    in Result datapath $ CElem e noPos
parserOfKind (Just ParseableXML) datapath = \file ->
    case xmlParse' "" $ B.unpack file of
         Left _ -> AccessError
         Right (Document _prolog _ e _) -> Result datapath $ CElem e noPos
parserOfKind (Just ParseableJson) datapath = DataBlob datapath

-- | Given a resource path, do the required loading
loadHtml :: (MonadIO m)
         => Loggers -> ResourcePath
         -> m (AccessResult HaXmLNode ResourcePath)
loadHtml (logger, _errLog, _verbose) datapath@(Local s) = do
    liftIO . logger $ "Opening file : '" ++ s ++ "'"
    realFile <- liftIO $ doesFileExist s
    if not realFile
       then return AccessError
       else do file <- liftIO $ B.readFile s
       	       let kind = getParseKind s
       	       return $ parserOfKind kind datapath file

loadHtml loggers@(logger, _, verbose) datapath@(Remote uri) = do
  liftIO . logger $ "Downloading URL : '" ++ show uri ++ "'"
  (u, rsp) <- downloadBinary loggers uri
  let contentType = retrieveHeaders HdrContentType rsp
  case contentType of
    [] -> return AccessError
    (hdr:_) ->
       let logString = "Downloaded (" ++ show u ++ ") ["
                        ++ hdrValue hdr ++ "] "
           
       	   kind = getParseKind (uriPath uri)
       in do liftIO $ verbose logString
             return . parserOfKind kind datapath $ rspBody rsp

