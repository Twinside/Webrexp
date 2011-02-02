{-# LANGUAGE ScopedTypeVariables #-}
-- | Generic module for using Webrexp as a user.
module Webrexp ( 
               -- * Default evaluation
                 evalWebRexp
               , evalWebRexpDepthFirst 
               , parseWebRexp
               , evalParsedWebRexp

               -- * Crawling configuration
               , Conf (..)
               , defaultConf
               , evalWebRexpWithConf
               ) where

import Control.Monad
import Text.Parsec
import System.IO

import Webrexp.Exprtypes
import Webrexp.Parser( webRexpParser )
import Webrexp.HxtNode
import Webrexp.ResourcePath
import Webrexp.WebContext
import qualified Webrexp.Eval as E

-- we need the instance
import Webrexp.HxtNode ()

import Webrexp.WebRexpAutomata


data Conf = Conf
    { hammeringDelay :: Int
    , userAgent :: String
    , output :: Handle
    , verbose :: Bool
    , quiet :: Bool
    , expr :: String
    , showHelp :: Bool
    , depthEvaluation :: Bool
    }

defaultConf :: Conf
defaultConf = Conf
    { hammeringDelay = 1500
    , userAgent = ""
    , output = stdout
    , verbose = False
    , quiet = False
    , expr = ""
    , showHelp = False
    , depthEvaluation = True
    }

type Crawled a = WebCrawler HxtNode ResourcePath a

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
 where crawled :: Crawled Bool = E.evalWebRexp wexpr

-- | Simple evaluation function, evaluation is
-- the breadth first type.
evalWebRexp :: String -> IO Bool
evalWebRexp = evalWebRexpWithEvaluator E.evalWebRexp

evalWebRexpDepthFirst :: String -> IO Bool
evalWebRexpDepthFirst = evalWebRexpWithEvaluator evalDepthFirst 

-- | Simplest function to eval a webrexp.
-- Return the evaluation status of the webrexp,
-- True for full evaluation success.
evalWebRexpWithEvaluator :: (WebRexp -> Crawled Bool) -> String -> IO Bool
evalWebRexpWithEvaluator evaluator str = 
  case runParser webRexpParser () "expr" str of
    Left err -> do
        putStrLn "Parsing error :\n"
        putStrLn $ show err
        return False

    Right wexpr ->
        let crawled :: Crawled Bool = evaluator wexpr
        in evalWithEmptyContext crawled

evalWebRexpWithConf :: Conf -> IO Bool
evalWebRexpWithConf conf =
  case runParser webRexpParser () "expr" (expr conf) of
    Left err -> do
        putStrLn "Parsing error :\n"
        putStrLn $ show err
        return False

    Right wexpr ->
        let crawled :: Crawled Bool = do
              setUserAgent $ userAgent conf
              setOutput $ output conf
              setHttpDelay $ hammeringDelay conf
              when (quiet conf) (setLogLevel Quiet)
              when (verbose conf) (setLogLevel Verbose)
              if depthEvaluation conf
              	 then evalDepthFirst wexpr
              	 else E.evalWebRexp wexpr

        in do rez <- evalWithEmptyContext crawled
              if output conf /= stdout
                then hClose $ output conf
                else return ()
              return rez

