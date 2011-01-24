{-# LANGUAGE TypeSynonymInstances #-}
module Webrexp.HxtNode( HxtNode ) where

import Control.Concurrent
import Network.HTTP
import Network.Browser

import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Parser.HtmlParsec
import Text.XML.HXT.DOM.TypeDefs

import System.Directory

import Webrexp.Log
import Webrexp.ResourcePath
import Webrexp.GraphWalker

import Debug.Trace

type HxtNode = NTree XNode

instance GraphWalker HxtNode where
    accessGraph = loadHtml
    attribOf = findAttribute 
    childrenOf = findChildren
    valueOf = getValue
    nameOf = getName

findAttribute :: String -> HxtNode -> Maybe String
findAttribute attrName (NTree (XTag _ attrList) _) =
   -- (\a -> trace ("]] " ++ attrName ++ "=" ++ show a) a) $ 
    attrFinder attrList
  where attrFinder [] = Nothing
        attrFinder (NTree (XAttr name) [value]:_)
            | localPart name == attrName = Just $ valueOf value
        attrFinder (_:xs) = attrFinder xs
findAttribute _ _ = Nothing

findChildren :: HxtNode -> [HxtNode]
findChildren (NTree (XTag _ _) children) = children
findChildren n = []

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
loadHtml :: ResourcePath -> IO (Maybe (ResourcePath, HxtNode))
loadHtml (Local s) = do
    infoLog $ "Opening file : '" ++ s ++ "'"
    infoLog $ "-------------------------------------"
    realFile <- doesFileExist s
    if not realFile
       then return Nothing
       else readFile s >>= (return . Just
                                  . (,) (Local s) 
                                  . parseToHTMLNode)

loadHtml (Remote uri) = do
  infoLog $ "-------------------------------------"
  infoLog $ "Downloading URL : '" ++ show uri ++ "'"
  threadDelay 1500
  (u, rsp) <- browse $ do
        setAllowRedirects True
        setErrHandler networkError
        setOutHandler networkInfo
        request $ defaultGETRequest uri

  infoLog $ "Downloaded (" ++ show uri ++ ")"
  infoLog $ "-------------------------------------"
  return . Just
         . (,) (Remote u) 
         . parseToHTMLNode $ rspBody rsp

