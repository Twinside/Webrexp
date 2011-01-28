-- | Module implementing plumbing to get a unified path locator,
-- handling URI & local path.
module Webrexp.ResourcePath 
    ( ResourcePath (..)
    , rezPathToString
    ) where

import Webrexp.GraphWalker
import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Maybe
import Network.HTTP
import Network.URI
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as B

data ResourcePath =
      Local FilePath
    | Remote URI
    deriving (Eq, Show)

instance GraphPath ResourcePath where
    (<//>) = combinePath
    importPath = toRezPath
    dumpDataAtPath = dumpResourcePath 
    localizePath = extractFileName

rezPathToString :: ResourcePath -> String
rezPathToString (Local p) = p
rezPathToString (Remote uri) = show uri

toRezPath :: String -> Maybe ResourcePath
toRezPath s = case (parseURI s, isValid s, isRelativeReference s) of
        (Just u, _, _) -> Just $ Remote u
        (Nothing, True, _) -> Just $ Local s
        (Nothing, False, True) -> Remote <$> parseRelativeReference s
        (Nothing, False, False) -> Nothing

-- | Resource path combiner, similar to </> in use,
-- but also handle URI.
combinePath :: ResourcePath -> ResourcePath -> ResourcePath
combinePath (Local a) (Local b) = Local $ (dropFileName a) </> b
combinePath (Remote a) (Remote b) =
    case b `relativeTo` a of
         -- TODO : find another way for this
         Nothing -> error "Can't merge resourcepath" 
                   -- Remote a
         Just c -> Remote c

combinePath (Remote a) (Local b)
    | isRelativeReference b = case parseRelativeReference b of
        Just r -> Remote . fromJust $ r `relativeTo` a
        Nothing -> error "Not possible, checked before"
combinePath (Local _) b@(Remote _) = b
combinePath _ _ = error "Mixing local/remote path"

extractFileName :: ResourcePath -> String
extractFileName (Remote a) = snd . splitFileName $ uriPath a
extractFileName (Local c) = snd $ splitFileName c

dumpResourcePath :: (Monad m, MonadIO m)
                 => (String -> m ()) -> ResourcePath -> m ()
dumpResourcePath _ src@(Local source) = do
    cwd <- liftIO $ getCurrentDirectory
    liftIO . copyFile source $ cwd </> extractFileName src

dumpResourcePath logger p@(Remote a) =
  downloadBinary logger a $ extractFileName p

downloadBinary :: (Monad m, MonadIO m)
               => (String -> m ()) -> URI -> FilePath -> m ()
downloadBinary logger url filename = do
    logger $ "Downloading '" ++ show url ++ "' in '" ++ filename
    liftIO $ threadDelay 1500
    rsp <- liftIO . Network.HTTP.simpleHTTP $ mkRequest GET url
    body <- liftIO $ getResponseBody rsp
    liftIO $ B.writeFile filename body

