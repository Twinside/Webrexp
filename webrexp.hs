{-# LANGUAGE ScopedTypeVariables #-}
-- | Generic module for using Webrexp as a user.
module Webrexp ( evalWebRexp
               , parseWebRexp
               , evalParsedWebRexp
               ) where

import Text.Parsec

import Webrexp.Exprtypes
import Webrexp.Parser( webRexpParser )
import Webrexp.HxtNode
import Webrexp.WebContext
import qualified Webrexp.Eval as E

-- we need the instance
import Webrexp.HxtNode()

-- | Prepare a webrexp.
-- This function is useful if the expression has
-- to be applied many times.
parseWebRexp :: String -> Maybe WebRexp
parseWebRexp str =
  case runParser webRexpParser () "expr" str of
       Left _ -> Nothing
       Right e -> Just e

-- | Evaluation for pre-parsed webrexp.
-- Best method if a webrexp has to be evaluated
-- many times.
evalParsedWebRexp :: WebRexp -> IO Bool
evalParsedWebRexp expr = evalWithEmptyContext crawled
 where crawled :: WebCrawler HxtNode Bool = 
            E.evalWebRexp True expr

-- | Simplest function to eval a webrexp.
-- Return the evaluation status of the webrexp,
-- True for full evaluation success.
evalWebRexp :: String -> IO Bool
evalWebRexp str = 
  case runParser webRexpParser () "expr" str of
    Left err -> do
        putStrLn "Parsing error :\n"
        putStrLn $ show err
        return False

    Right expr ->
        let crawled :: WebCrawler HxtNode Bool = 
                E.evalWebRexp True expr
        in do putStrLn $ "Parsed: " ++ show expr
              evalWithEmptyContext crawled

