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
import Control.Monad.IO.Class
import Text.Parsec
import System.IO
import System.Exit

import Webrexp.Exprtypes
import Webrexp.Parser( webRexpParser )

import Webrexp.HaXmlNode
import Webrexp.HxtNode
import Webrexp.JsonNode
import Webrexp.UnionNode
import Webrexp.DirectoryNode

import Webrexp.ResourcePath
import Webrexp.WebContext

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
    , outputGraphViz :: Bool
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
    , outputGraphViz = False
    , depthEvaluation = True
    }

type CrawledNode =
    UnionNode (UnionNode  HxtNode  HaXmLNode)
              (UnionNode JsonNode DirectoryNode)

type Crawled a =
            WebCrawler CrawledNode ResourcePath a

initialState :: IO (EvalState CrawledNode ResourcePath)
initialState = do
    node <- currentDirectoryNode 
    return . Node $ repurposeNode (UnionRight . UnionRight) node

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
 where crawled :: Crawled Bool = evalBreadthFirst (Text "") wexpr

-- | Simple evaluation function, evaluation is
-- the breadth first type.
evalWebRexp :: String -> IO Bool
evalWebRexp = evalWebRexpWithEvaluator $ evalBreadthFirst (Text "")

evalWebRexpDepthFirst :: String -> IO Bool
evalWebRexpDepthFirst = evalWebRexpWithEvaluator $ evalDepthFirst (Text "")

-- | Simplest function to eval a webrexp.
-- Return the evaluation status of the webrexp,
-- True for full evaluation success.
evalWebRexpWithEvaluator :: (WebRexp -> Crawled Bool) -> String -> IO Bool
evalWebRexpWithEvaluator evaluator str = 
  case runParser webRexpParser () "expr" str of
    Left err -> do
        putStrLn "Parsing error :\n"
        print err
        return False

    Right wexpr ->
        let crawled :: Crawled Bool = evaluator wexpr
        in evalWithEmptyContext crawled

evalWebRexpWithConf :: Conf -> IO Bool
evalWebRexpWithConf conf =
  case runParser webRexpParser () "expr" (expr conf) of
    Left err -> do
        putStrLn "Parsing error :\n"
        print err
        return False

    Right wexpr -> do
        when (outputGraphViz conf)
             (do let packed = packRefFiltering wexpr
                 dumpAutomata (expr conf) stdout $ buildAutomata packed
                 exitWith ExitSuccess)

        when (verbose conf) (print wexpr)

        let crawled :: Crawled Bool = do
              setUserAgent $ userAgent conf
              setOutput $ output conf
              setHttpDelay $ hammeringDelay conf
              when (quiet conf) (setLogLevel Quiet)
              when (verbose conf) (setLogLevel Verbose)
              initState <- liftIO initialState
              if depthEvaluation conf
              	 then evalDepthFirst initState wexpr
              	 else evalBreadthFirst initState wexpr

        rez <- evalWithEmptyContext crawled

        when (output conf /= stdout)
             (hClose $ output conf)

        return rez

