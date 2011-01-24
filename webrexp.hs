{-# LANGUAGE ScopedTypeVariables #-}
-- | Generic module for using Webrexp as a user.
module Webrexp ( 
               -- * Default evaluation
                 evalWebRexp
               , parseWebRexp
               , evalParsedWebRexp

               -- * Crawling configuration
               , Conf (..)
               , defaultConf
               , evalWebRexpWithConf
               ) where

import Control.Monad
import Control.Monad.IO.Class
import Text.Parsec
import System.IO

import Webrexp.Exprtypes
import Webrexp.Parser( webRexpParser )
import Webrexp.HxtNode
import Webrexp.WebContext
import qualified Webrexp.Eval as E

-- we need the instance
import Webrexp.HxtNode()

data Conf = Conf
    { hammeringDelay :: Int
    , userAgent :: String
    , output :: Handle
    , verbose :: Bool
    , quiet :: Bool
    , expr :: String
    }

defaultConf :: Conf
defaultConf = Conf
    { hammeringDelay = 1500
    , userAgent = ""
    , output = stdout
    , verbose = False
    , quiet = False
    , expr = ""
    }

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
evalParsedWebRexp wexpr = evalWithEmptyContext crawled
 where crawled :: WebCrawler HxtNode Bool = 
            E.evalWebRexp True wexpr

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

    Right wexpr ->
        let crawled :: WebCrawler HxtNode Bool = 
                E.evalWebRexp True wexpr
        in do putStrLn $ "Parsed: " ++ show wexpr
              evalWithEmptyContext crawled

evalWebRexpWithConf :: Conf -> IO Bool
evalWebRexpWithConf conf =
  case runParser webRexpParser () "expr" (expr conf) of
    Left err -> do
        putStrLn "Parsing error :\n"
        putStrLn $ show err
        return False

    Right wexpr ->
        let crawled :: WebCrawler HxtNode Bool = do
              setUserAgent $ userAgent conf
              setOutput $ output conf
              setHttpDelay $ hammeringDelay conf
              when (quiet conf) (setLogLevel Quiet)
              when (verbose conf) (setLogLevel Verbose)
              E.evalWebRexp True wexpr

        in do liftIO . putStrLn $ "Parsed: " ++ show wexpr
              rez <- evalWithEmptyContext crawled
              hClose $ output conf
              return rez

