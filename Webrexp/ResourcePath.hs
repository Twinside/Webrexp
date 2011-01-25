-- | Module implementing plumbing to get a unified path locator,
-- handling URI & local path.
module Webrexp.ResourcePath 
    ( ResourcePath (..)
    , toRezPath
    , (<//>)
    , dumpResourcePath
    , rezPathToString
    ) where

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

rezPathToString :: ResourcePath -> String
rezPathToString (Local p) = p
rezPathToString (Remote uri) = show uri

toRezPath :: String -> Maybe ResourcePath
toRezPath s = case (parseURI s, isValid s) of
        (Just u, _) -> Just $ Remote u
        (Nothing, True) -> Just $ Local s
        (Nothing, False) -> Nothing

-- | Resource path combiner, similar to </> in use,
-- but also handle URI.
(<//>) :: ResourcePath -> ResourcePath -> ResourcePath
(<//>) (Local a) (Local b) = Local $ (dropFileName a) </> b
(<//>) (Remote a) (Remote b) =
    case b `relativeTo` a of
         -- TODO : find another way for this
         Nothing -> error "Can't merge resourcepath" 
                   -- Remote a
         Just c -> Remote c

(<//>) (Remote a) (Local b)
    | isRelativeReference b = case parseRelativeReference b of
        Just r -> Remote . fromJust $ r `relativeTo` a
        Nothing -> error "Not possible, checked before"
(<//>) (Local _) b@(Remote _) = b
(<//>) _ _ = error "Mixing local/remote path"

dumpResourcePath :: (Monad m, MonadIO m)
                 => (String -> m ()) -> ResourcePath -> m ()
dumpResourcePath _ (Local source) = do
    cwd <- liftIO $ getCurrentDirectory
    liftIO . copyFile source $ cwd </> filename
     where (_, filename) = splitFileName source

dumpResourcePath logger (Remote a) =
  downloadBinary logger a filename
    where (_, filename) = splitFileName $ uriPath a

downloadBinary :: (Monad m, MonadIO m)
               => (String -> m ()) -> URI -> FilePath -> m ()
downloadBinary logger url filename = do
    logger $ "Downloading '" ++ show url ++ "' in '" ++ filename
    liftIO $ threadDelay 1500
    rsp <- liftIO . Network.HTTP.simpleHTTP $ mkRequest GET url
    body <- liftIO $ getResponseBody rsp
    liftIO $ B.writeFile filename body

