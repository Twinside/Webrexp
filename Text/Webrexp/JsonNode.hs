{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Webrexp.JsonNode( JsonNode ) where

import Control.Arrow
import Control.Applicative
import Control.Monad.IO.Class
import Data.Maybe( catMaybes )
import qualified Data.Map as Map
import Network.HTTP
import System.Directory
import Text.JSON.AttoJSON

import qualified Text.Webrexp.ProjectByteString as B

import Text.Webrexp.GraphWalker
import Text.Webrexp.ResourcePath
import Text.Webrexp.UnionNode
import Text.Webrexp.Remote.MimeTypes

type JsonNode = (Maybe String, JSValue)

instance PartialGraph JsonNode ResourcePath where
    isResourceParseable _ _ ParseableJson = True
    isResourceParseable _ _ _ = False

    parseResource _ _ ParseableJson binData = return $ (,) Nothing <$> readJSON binData
    parseResource _ _ _ _ = error "Wrong kind of parser used"

instance GraphWalker JsonNode ResourcePath where
    accessGraph = loadJson

    attribOf attrName (_, JSObject obj) =
        valueOf . none <$> Map.lookup (B.pack attrName) obj
            where none a = (Nothing :: Maybe String, a)
    attribOf _ _ = Nothing

    childrenOf (_, JSArray children) =
        return $ (,) Nothing <$> children
    childrenOf (_, JSObject obj) =
        return $ first (Just . B.unpack) <$> Map.assocs obj
    childrenOf _ = return []

    nameOf (Just s, _) = Just s
    nameOf _ = Nothing

    indirectLinks n =
        catMaybes [ attribOf "href" n >>= importPath
                  , attribOf "src" n >>= importPath ]

    isHistoryMutable _ = False

    valueOf (_, JSString s) = B.unpack s
    valueOf (_, JSNumber i) = show i
    valueOf (_, JSBool b) = show b
    valueOf (_, JSArray _) = ""
    valueOf (_, JSObject _) = ""
    valueOf (_, JSNull) = ""

parseJson :: (MonadIO m)
          => Loggers -> ResourcePath -> B.ByteString
          -> m (AccessResult JsonNode ResourcePath)
parseJson (_, errLog, _) datapath file =
    case parseJSON file of
      Left err -> do liftIO . errLog $ "> JSON Parsing error " ++ err
       	             return AccessError
      Right valid -> return $ Result datapath (Nothing, valid)

-- | Given a resource path, do the required loading
loadJson :: (MonadIO m)
         => Loggers -> ResourcePath
         -> m (AccessResult JsonNode ResourcePath)
loadJson loggers@(logger, _errLog, _verbose) datapath@(Local s) = do
    liftIO . logger $ "Opening file : '" ++ s ++ "'"
    realFile <- liftIO $ doesFileExist s
    if not realFile
       then return AccessError
       else do file <- liftIO $ B.readFile s
       	       parseJson loggers datapath file

loadJson loggers@(logger, _, verbose) datapath@(Remote uri) = do
  liftIO . logger $ "Downloading URL : '" ++ show uri ++ "'"
  (u, rsp) <- downloadBinary loggers uri
  let contentType = retrieveHeaders HdrContentType rsp
  case contentType of
    [] -> return AccessError
    (hdr:_) ->
       do liftIO . verbose $ "Downloaded (" ++ show u ++ ") ["
                                ++ hdrValue hdr ++ "] "
          parseJson loggers datapath $ rspBody rsp

