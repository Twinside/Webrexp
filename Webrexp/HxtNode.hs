{-# LANGUAGE TypeSynonymInstances #-}
module Webrexp.HxtNode( HxtNode ) where

import Control.Monad.IO.Class
import Network.HTTP
import Network.Browser

import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Parser.HtmlParsec
import Text.XML.HXT.DOM.TypeDefs

import System.Directory

import Webrexp.ResourcePath
import Webrexp.GraphWalker

type HxtNode = NTree XNode

instance GraphWalker HxtNode where
    accessGraph = loadHtml
    attribOf = findAttribute 
    childrenOf = findChildren
    valueOf = getValue
    nameOf = getName

findAttribute :: String -> HxtNode -> Maybe String
findAttribute attrName (NTree (XTag _ attrList) _) =
    attrFinder attrList
  where attrFinder [] = Nothing
        attrFinder (NTree (XAttr name) [value]:_)
            | localPart name == attrName = Just $ valueOf value
        attrFinder (_:xs) = attrFinder xs
findAttribute _ _ = Nothing

findChildren :: HxtNode -> [HxtNode]
findChildren (NTree (XTag _ _) children) = children
findChildren _ = []

getName :: HxtNode -> Maybe String
getName (NTree (XTag name _) _) = Just $ localPart name
getName _ = Nothing

getValue :: HxtNode -> String
getValue (NTree (XText txt) _) = txt
getValue _ = ""

parseToHTMLNode :: String -> HxtNode
parseToHTMLNode txt = case findFirstNamed "html" nodes of
                        Nothing -> NTree (XTag (mkName "html") []) nodes
                        Just (d, _) -> d
    where nodes = parseHtmlContent txt

-- | Given a resource path, do the required loading
loadHtml :: (MonadIO m)
         => Logger -> Logger -> Logger -> ResourcePath
         -> m (Maybe (ResourcePath, HxtNode))
loadHtml logger _errLog _verbose (Local s) = do
    liftIO . logger $ "Opening file : '" ++ s ++ "'"
    realFile <- liftIO $ doesFileExist s
    if not realFile
       then return Nothing
       else do file <- liftIO $ readFile s
       	       return . Just . (,) (Local s) 
                             $ parseToHTMLNode file

loadHtml logger errLog verbose (Remote uri) = do
  liftIO . logger $ "Downloading URL : '" ++ show uri ++ "'"
  (u, rsp) <- liftIO . browse $ do
        setAllowRedirects True
        setErrHandler errLog
        setOutHandler verbose
        request $ defaultGETRequest uri

  liftIO . verbose $ "Downloaded (" ++ show uri ++ ")"
  return . Just
         . (,) (Remote u) 
         . parseToHTMLNode $ rspBody rsp

