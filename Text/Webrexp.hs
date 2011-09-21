{-# LANGUAGE ScopedTypeVariables #-}
-- | Generic module for using Webrexp as a user.
-- the main functions for the user are queryDocument to perform an in-memory
-- evaluation, and evalWebRexpDepthFirst
module Text.Webrexp ( 
               -- * In memory evaluation
                 ParseableType( .. )
               , queryDocument
               , queryDocumentM

               -- * Default evaluation
               , evalWebRexp
               , evalWebRexpDepthFirst
               , parseWebRexp
               , evalParsedWebRexp
               , executeParsedWebRexp 

               -- * Crawling configuration
               , Conf (..)
               , defaultConf
               , evalWebRexpWithConf
               ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.ST
import Text.Parsec
import System.IO
import System.Exit

import Data.Array.ST
import Data.Array.IO

import Text.Webrexp.Exprtypes
import Text.Webrexp.Parser( webRexpParser )

import Text.Webrexp.HaXmlNode
import Text.Webrexp.HxtNode
import Text.Webrexp.JsonNode
import Text.Webrexp.UnionNode
import Text.Webrexp.DirectoryNode

import Text.Webrexp.GraphWalker
import Text.Webrexp.ResourcePath
import Text.Webrexp.WebContext
import Text.Webrexp.WebRexpAutomata
import Text.Webrexp.Remote.MimeTypes
import qualified Text.Webrexp.ProjectByteString as B

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

type Crawled a = WebCrawler IOArray CrawledNode ResourcePath a
type MemoryCrawl s a = WebContextT (STArray s) CrawledNode ResourcePath (ST s) a

initialState :: IO (EvalState CrawledNode ResourcePath)
initialState = do
    node <- currentDirectoryNode 
    return . Node $ repurposeNode (UnionRight . UnionRight) node

-- | Query a document in memory and retrieve the results, you can use it in combination
-- to the quasiquoting facility to embed the webrexp in haskell :
--
-- > {-# LANGUAGE QuasiQuotes #-}
-- > import Text.Webrexp
-- > import Text.Webrexp.Quote
-- > import qualified Data.ByteString.Char8 as B
-- >
-- > main :: IO ()
-- > main = print $ queryDocument ParseableJson document [webrexpParse| some things [.] |]
-- >     where document = B.pack "{ \"some\": { \"things\": \"a phrase\" } }"
-- 
-- The returned values contain possible errors as 'Left' and real value as 'Right.
--
queryDocument :: ParseableType -> B.ByteString -> WebRexp -> [Either String String]
queryDocument docType str query = runST $ queryDocumentM docType str query

-- | Exactly same thing as 'queryDocument', but in ST
queryDocumentM :: forall s . ParseableType -> B.ByteString -> WebRexp 
               -> ST s [Either String String]
queryDocumentM docType str query = executeWithEmptyContext todo
    where ignoreLog _ = return ()
          loggers = (ignoreLog, ignoreLog, ignoreLog)
          todo :: MemoryCrawl s Bool
          todo = do
            initialNode <- parseUnion loggers (Just docType) (Local "") str
            case initialNode of
                AccessError -> return False
                DataBlob _ _ -> return False
                Result rezPath a -> 
                    let initNode = NodeContext { rootRef = rezPath
                                               , this = a
                                               , parents = ImmutableHistory [] }
                    in do setLogLevel Quiet
                          evalDepthFirst (Node initNode) query

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
-- many times. Evaluated using breadth first method.
evalParsedWebRexp :: WebRexp -> IO Bool
evalParsedWebRexp wexpr = evalWithEmptyContext crawled
 where crawled :: Crawled Bool = evalBreadthFirst (Text "") wexpr

-- | Evaluate a webrexp and return all the dumped text as 'Right'
-- and all errors as 'Left'. Evaluated using depth first method.
executeParsedWebRexp :: WebRexp -> IO [Either String String]
executeParsedWebRexp wexpr = executeWithEmptyContext crawled
 where crawled :: Crawled Bool = evalDepthFirst (Text "") wexpr

-- | Simple evaluation function, evaluation is
-- the breadth first type.
evalWebRexp :: String -> IO Bool
evalWebRexp = evalWebRexpWithEvaluator $ evalBreadthFirst (Text "")

-- | Evaluate a webrexp in depth first fashion, returning a success
-- status telling if the evaluation got up to the end.
evalWebRexpDepthFirst :: String -> IO Bool
evalWebRexpDepthFirst = evalWebRexpWithEvaluator $ evalDepthFirst (Text "")

-- | Simplest function to eval a webrexp.
-- Return the evaluation status of the webrexp,
-- True for full evaluation success.
evalWebRexpWithEvaluator :: (WebRexp -> Crawled Bool) -> String -> IO Bool
evalWebRexpWithEvaluator evaluator str = 
  case runParser webRexpParser () "expr" str of
    Left err -> do
        putStrLn $ "Parsing error :\n" ++ show err
        return False

    Right wexpr ->
        let crawled :: Crawled Bool = evaluator wexpr
        in evalWithEmptyContext crawled

-- | Function used in the command line program.
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

        when (verbose conf) 
             (do putStrLn $ "code " ++ show (expr conf)
                 print wexpr)

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

