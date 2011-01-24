-- | Module implementing plumbing to get a unified path locator,
-- handling URI & local path.
module Webrexp.ResourcePath 
    ( ResourcePath (..)
    , toRezPath
    , (<//>)
    , dumpResourcePath
    ) where

import Control.Concurrent
import Data.Maybe
import Network.HTTP
import Network.URI
import System.Directory
import System.FilePath
import qualified Data.ByteString.Lazy as B

import Webrexp.Log


data ResourcePath =
      Local FilePath
    | Remote URI
    deriving (Eq, Show)

toRezPath :: String -> Maybe ResourcePath
toRezPath s = case (parseURI s, isValid s) of
        (Just u, _) -> Just $ Remote u
        (Nothing, True) -> Just $ Local s
        (Nothing, False) -> Nothing

-- | Resource path combiner, similar to </> in use,
-- but also handle URI.
(<//>) :: ResourcePath -> ResourcePath -> ResourcePath
(<//>) (Local a) (Local b) = Local $ a </> b
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

(<//>) _ _ = error "Mixing local/remote path"

dumpResourcePath :: ResourcePath -> IO ()
dumpResourcePath (Local source) = do
    cwd <- getCurrentDirectory
    copyFile source $ cwd </> filename
     where (_, filename) = splitFileName source

dumpResourcePath (Remote a) =
  downloadBinary a filename
    where (_, filename) = splitFileName $ uriPath a

downloadBinary :: URI -> FilePath -> IO ()
downloadBinary url filename = do
    infoLog $ "Downloading '" ++ show url ++ "' in '" ++ filename
    threadDelay 500
    rsp <- Network.HTTP.simpleHTTP $ mkRequest GET url
    body <- getResponseBody rsp
    B.writeFile filename body

