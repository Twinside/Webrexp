{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.Webrexp.JsonNode( JsonNode ) where

import Control.Arrow
import Control.Applicative
import Data.Maybe( catMaybes )
import qualified Data.HashMap.Strict as Map
import Network.HTTP
import System.Directory
import Data.Aeson( decode
                 , Value( Object
                        , Array
                        , String
                        , Number
                        , Bool
                        , Null)
                 )

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Text.Webrexp.ProjectByteString as B
import qualified Data.ByteString.Lazy.Char8 as L

import Text.Webrexp.IOMock
import Text.Webrexp.GraphWalker
import Text.Webrexp.ResourcePath
import Text.Webrexp.UnionNode
import Text.Webrexp.Remote.MimeTypes

type JsonNode = (Maybe String, Value)

instance PartialGraph JsonNode ResourcePath where
    isResourceParseable _ _ ParseableJson = True
    isResourceParseable _ _ _ = False

    parseResource _ _ ParseableJson binData =
        return $ (,) Nothing <$> decode (L.fromChunks [binData])
    parseResource _ _ _ _ = error "Wrong kind of parser used"

instance GraphWalker JsonNode ResourcePath where
    accessGraph = loadJson
    rawAccess = accessResourcePath

    attribOf attrName (_, Object obj) =
        valueOf . none <$> Map.lookup (T.pack attrName) obj
            where none a = (Nothing :: Maybe String, a)
    attribOf _ _ = Nothing

    childrenOf (_, Array children) =
        return $ (,) Nothing <$> V.toList children
    childrenOf (_, Object obj) =
        return $ first (Just . T.unpack) <$> Map.toList obj
    childrenOf _ = return []

    nameOf (Just s, _) = Just s
    nameOf _ = Nothing

    indirectLinks n =
        catMaybes [ attribOf "href" n >>= importPath
                  , attribOf "src" n >>= importPath ]

    isHistoryMutable _ = False

    valueOf (_, String s) = T.unpack s
    valueOf (_, Number i) = show i
    valueOf (_, Bool b) = show b
    valueOf (_, Array _) = ""
    valueOf (_, Object _) = ""
    valueOf (_, Null) = ""

parseJson :: (Monad m) => Loggers m -> ResourcePath -> B.ByteString
          -> m (AccessResult JsonNode ResourcePath)
parseJson (_, errLog, _) datapath file =
    case decode $ L.fromChunks [file] of
      Nothing    -> do errLog "> JSON Parsing error"
                       return AccessError
      Just valid -> return $ Result datapath (Nothing, valid)

-- | Given a resource path, do the required loading
loadJson :: (IOMockable m, Monad m)
         => Loggers m -> ResourcePath
         -> m (AccessResult JsonNode ResourcePath)
loadJson loggers@(logger, _errLog, _verbose) datapath@(Local s) = do
    logger $ "Opening file : '" ++ s ++ "'"
    realFile <- performIO $ doesFileExist s
    case realFile of
        Just True -> performIO (B.readFile s) >>=
                maybe (return AccessError)
                      (\file -> parseJson loggers datapath file)

        _         -> return AccessError

loadJson loggers@(logger, _, verbose) datapath@(Remote uri) = do
  logger $ "Downloading URL : '" ++ show uri ++ "'"
  (u, rsp) <- downloadBinary loggers uri
  let contentType = retrieveHeaders HdrContentType rsp
  case contentType of
    [] -> return AccessError
    (hdr:_) ->
       do verbose $ "Downloaded (" ++ show u ++ ") ["
                                ++ hdrValue hdr ++ "] "
          parseJson loggers datapath $ rspBody rsp

