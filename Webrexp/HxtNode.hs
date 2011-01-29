{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Webrexp.HxtNode( HxtNode ) where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Tree.NTree.TypeDefs
import Network.HTTP
import System.Directory
import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.Parser.HtmlParsec

import qualified Data.ByteString.Lazy.Char8 as B ( unpack )

import Webrexp.ResourcePath
import Webrexp.GraphWalker
import Webrexp.Remote.MimeTypes


type HxtNode = NTree XNode

instance GraphWalker HxtNode ResourcePath where
    accessGraph = loadHtml
    attribOf = findAttribute 
    childrenOf = findChildren
    valueOf = valueOfNode
    nameOf = getName
    indirectLinks = hyperNode

valueOfNode :: HxtNode -> String
valueOfNode (NTree (XText txt) _) = txt
valueOfNode a =
    case childrenOf a of
        (NTree (XText txt) _:_) -> txt
        _ -> ""

extractText :: [HxtNode] -> String
extractText = concat . map valueOfNode

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
parseToHTMLNode txt = case findFirstNamed "html" nodes of
                        Nothing -> NTree (XTag (mkName "html") []) nodes
                        Just (d, _) -> d
    where nodes = parseHtmlContent txt

-- | Given a resource path, do the required loading
loadHtml :: (MonadIO m)
         => Loggers -> ResourcePath
         -> m (AccessResult HxtNode ResourcePath)
loadHtml (logger, _errLog, _verbose) (Local s) = do
    liftIO . logger $ "Opening file : '" ++ s ++ "'"
    realFile <- liftIO $ doesFileExist s
    if not realFile
       then return AccessError
       else do file <- liftIO $ readFile s
       	       return . Result (Local s)
                      $ parseToHTMLNode file

loadHtml loggers@(logger, _,  verbose) (Remote uri) = do
  liftIO . logger $ "Downloading URL : '" ++ show uri ++ "'"
  (u, rsp) <- downloadBinary loggers uri
  let contentType = retrieveHeaders HdrContentType rsp
  case contentType of
    [] -> return AccessError
    (hdr:_) ->
       let logString = "Downloaded (" ++ show u ++ ") ["
                        ++ hdrValue hdr ++ "] "
       in if isParseable $ hdrValue hdr
              then do
                liftIO . verbose $ logString ++ "parsed"
                let parsed = parseToHTMLNode 
                            . B.unpack $ rspBody rsp
                return $ Result (Remote u) parsed

              else do
                liftIO . verbose $ logString ++ "Binary Blob"
                return . DataBlob (Remote u) $ rspBody rsp

