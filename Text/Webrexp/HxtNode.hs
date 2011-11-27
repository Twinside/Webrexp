{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module implement the GraphWalker type class
-- for node of HxT (Haskell Xml parser).
module Text.Webrexp.HxtNode ( HxtNode ) where

import Data.Maybe
import Data.Tree.NTree.TypeDefs
import Network.HTTP
import Network.URI
import System.Directory
import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.Parser.HtmlParsec

import qualified Text.Webrexp.ProjectByteString as B

import Text.Webrexp.IOMock
import Text.Webrexp.GraphWalker
import Text.Webrexp.ResourcePath
import Text.Webrexp.Remote.MimeTypes
import Text.Webrexp.UnionNode

-- | This type is an instance of 'GraphWalker'
-- with 'ResourcePath' as 'GraphPath'
type HxtNode = NTree XNode

instance PartialGraph HxtNode ResourcePath where
    isResourceParseable _ _ ParseableHTML = True
    isResourceParseable _ _ _ = False

    parseResource _ _ ParseableHTML bindata =
        return . Just . parseToHTMLNode $ B.unpack bindata
    parseResource _ _ _ _ = error "Cannot parse"


instance GraphWalker HxtNode ResourcePath where
    deepValueOf = return . deepValue
    isHistoryMutable _ = False
    accessGraph = loadHtml
    attribOf = findAttribute 
    childrenOf = return . findChildren
    valueOf = valueOfNode
    nameOf = getName
    indirectLinks = hyperNode
    rawAccess = accessResourcePath

deepValue :: HxtNode -> String
deepValue (NTree (XText txt) _) = txt
deepValue a = concatMap deepValue $ findChildren a

valueOfNode :: HxtNode -> String
valueOfNode (NTree (XText txt) _) = txt
valueOfNode a =
    case findChildren a of
        (NTree (XText txt) _:_) -> txt
        _ -> ""

extractText :: [HxtNode] -> String
extractText = concatMap valueOfNode

findAttribute :: String -> HxtNode -> Maybe String
findAttribute attrName (NTree (XTag _ attrList) _) =
    attrFinder attrList
  where attrFinder [] = Nothing
        attrFinder (NTree (XAttr name) values:_)
            | localPart name == attrName = Just $ extractText values
        attrFinder (_:xs) = attrFinder xs
findAttribute _ _ = Nothing

hyperNode :: HxtNode -> [ResourcePath]
hyperNode n = catMaybes [ attribOf "href" n >>= importPath
                        , attribOf "src" n >>= importPath]

findChildren :: HxtNode -> [HxtNode]
findChildren (NTree (XTag _ _) children) = children
findChildren _ = []

getName :: HxtNode -> Maybe String
getName (NTree (XTag name _) _) = Just $ localPart name
getName _ = Nothing

parseToHTMLNode :: String -> HxtNode
parseToHTMLNode txt = case findFirstNamedPure findChildren "html" nodes of
                        Nothing -> NTree (XTag (mkName "html") []) nodes
                        Just (d, _) -> d
    where nodes = parseHtmlContent txt

parserOfKind :: Maybe ParseableType
             -> ResourcePath
             -> B.ByteString -> AccessResult HxtNode ResourcePath
parserOfKind (Just ParseableHTML) datapath =
    Result datapath . parseToHTMLNode . B.unpack
parserOfKind _ datapath = DataBlob datapath

-- | Given a resource path, do the required loading
loadHtml :: (Monad m, IOMockable m)
         => Loggers m -> ResourcePath
         -> m (AccessResult HxtNode ResourcePath)
loadHtml (logger, _errLog, _verbose) (Local s) = do
    logger $ "Opening file : '" ++ s ++ "'"
    realFile <- performIO $ doesFileExist s
    case realFile of
        Just True -> performIO (readFile s) >>=
            maybe (return AccessError)
                  (return . Result (Local s) . parseToHTMLNode)
        _         -> return AccessError

loadHtml loggers@(logger, _,  verbose) datapath@(Remote uri) = do
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

