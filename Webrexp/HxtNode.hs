{-# LANGUAGE TypeSynonymInstances #-}
module Webrexp.HxtNode where

import Network.HTTP

import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Parser.HtmlParsec
import Text.XML.HXT.DOM.TypeDefs

import System.Directory

import Webrexp.ResourcePath
import Webrexp.GraphWalker

type HxtNode = [NTree XNode]

instance GraphWalker HxtNode where
    accessGraph = loadHtml
    attribOf = findAttribute 
    childrenOf = findChildren
    valueOf = getValue

findAttribute :: HxtNode -> String -> Maybe String
findAttribute _ _ = Nothing

findChildren :: HxtNode -> [HxtNode]
findChildren _ = []

getValue :: HxtNode -> String
getValue _ = ""

-- | Given a resource path, do the required loading
loadHtml :: ResourcePath -> IO (Maybe HxtNode)
loadHtml (Local s) = do
    realFile <- doesFileExist s
    if not realFile
       then return Nothing
       else readFile s >>=
                (return . Just . parseHtmlContent)

loadHtml (Remote uri) = do
  rsp <- Network.HTTP.simpleHTTP (mkRequest GET uri)
  case rsp of
    Left _err -> return Nothing
    Right resp -> return . Just 
                        . parseHtmlContent 
                        $ rspBody resp

