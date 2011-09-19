{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Webrexp.HaXmlNode( HaXmLNode ) where

import Control.Applicative
import Data.Maybe( catMaybes )
import Data.List( find )
import Network.HTTP
import Network.URI
import Text.XML.HaXml.Types
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Html.Parse
import System.Directory

import qualified Text.Webrexp.ProjectByteString as B

import Text.Webrexp.IOMock
import Text.Webrexp.GraphWalker
import Text.Webrexp.ResourcePath
import Text.Webrexp.Remote.MimeTypes
import Text.Webrexp.UnionNode

type HaXmLNode = Content Posn

instance PartialGraph HaXmLNode ResourcePath where
    isResourceParseable _ _ ParseableXML = True
    isResourceParseable _ _ _ = False

    parseResource _ _ ParseableXML bindata =
        case xmlParse' "" $ B.unpack bindata of
            Left _ -> return Nothing
            Right (Document _prolog _ e _) -> return . Just $ CElem e noPos
    parseResource _ _ _ _ = error "Cannot parse"

haxmlNameToString :: QName -> String
haxmlNameToString (N n) = n
haxmlNameToString (QN _ n) = n

instance GraphWalker HaXmLNode ResourcePath where
    accessGraph = loadHtml


    attribOf attrName (CElem (Elem _ attrList _) _) =
        show . snd <$> find nameFinder attrList
            where nameFinder (n,_) = haxmlNameToString n == attrName
    attribOf _ _ = Nothing

    childrenOf = return . pureChildren

    nameOf (CElem (Elem n _ _) _) = Just $ haxmlNameToString n
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
loadHtml :: (Monad m, IOMockable m)
         => Loggers m -> ResourcePath
         -> m (AccessResult HaXmLNode ResourcePath)
loadHtml (logger, _errLog, _verbose) datapath@(Local s) = do
    logger $ "Opening file : '" ++ s ++ "'"
    realFile <- performIO $ doesFileExist s
    case realFile of
        Just True -> performIO (B.readFile s) >>=
            maybe (return AccessError)
                  (let kind = getParseKind s 
                   in return . parserOfKind kind datapath)
        _         -> return AccessError

loadHtml loggers@(logger, _, verbose) datapath@(Remote uri) = do
  logger $ "Downloading URL : '" ++ show uri ++ "'"
  (u, rsp) <- downloadBinary loggers uri
  let contentType = retrieveHeaders HdrContentType rsp
  case contentType of
    [] -> return AccessError
    (hdr:_) ->
       let logString = "Downloaded (" ++ show u ++ ") ["
                        ++ hdrValue hdr ++ "] "
           
       	   kind = getParseKind (uriPath uri)
       in do verbose logString
             return . parserOfKind kind datapath $ rspBody rsp

