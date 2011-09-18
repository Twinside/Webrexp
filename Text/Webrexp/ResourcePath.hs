-- | Module implementing plumbing to get a unified path locator,
-- handling URI & local path. Implement the 'GraphPath' and 'GraphWalker'
-- typeclass with 'HxtNode'
module Text.Webrexp.ResourcePath 
    ( ResourcePath (..)
    , rezPathToString
    , downloadBinary
    ) where

import Text.Webrexp.GraphWalker
import Control.Applicative
import Control.Monad.IO.Class
import Data.Maybe
import Network.HTTP
import Network.Browser
import Network.URI
import System.Directory
import System.FilePath
import qualified Text.Webrexp.ProjectByteString as B

import Text.Webrexp.Remote.MimeTypes

-- | Main data type
data ResourcePath =
    -- | Represent a file stored on the hard-drive of this
    -- machine.
      Local FilePath
    -- | Represent a ressource spread on internet.
    | Remote URI
    deriving (Eq, Show)

instance GraphPath ResourcePath where
    (<//>) = combinePath
    importPath = toRezPath
    dumpDataAtPath = dumpResourcePath 
    localizePath = extractFileName

-- | Given a ressource, transforme it to a string
-- representation. This function should be used instead
-- of the 'Show' instance, which is aimed at debugging
-- only.
rezPathToString :: ResourcePath -> String
rezPathToString (Local p) = p
rezPathToString (Remote uri) = show uri

toRezPath :: String -> Maybe ResourcePath
toRezPath s = case (parseURI s, isValid s, isRelativeReference s) of
        (Just u, _, _) -> Just $ Remote u
        (Nothing, True, _) -> Just . Local $ normalise s
        (Nothing, False, True) -> Remote <$> parseRelativeReference s
        (Nothing, False, False) -> Nothing

-- | Resource path combiner, similar to </> in use,
-- but also handle URI.
combinePath :: ResourcePath -> ResourcePath -> ResourcePath
combinePath (Local a) (Local b) = Local . normalise $ dropFileName a </> b
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
                 => Loggers m -> ResourcePath -> m ()
dumpResourcePath _ src@(Local source) = do
    cwd <- liftIO getCurrentDirectory
    liftIO . copyFile source $ cwd </> extractFileName src

dumpResourcePath loggers@(logger,_,_) p@(Remote a) = do
  (_, rsp) <- downloadBinary loggers a
  let rawFilename = extractFileName p
      filename = case retrieveHeaders HdrContentType rsp of
         [] -> rawFilename 
         (hdr:_) -> addContentTypeExtension
                            (hdrValue hdr) rawFilename
                 

  logger $ "Downloading '" ++ show a ++ "' in '" ++ filename
  liftIO . B.writeFile filename $ rspBody rsp

-- | Helper function to grab a resource on internet and returning
-- it's binary representation, and it's real place if any.
downloadBinary :: (Monad m, MonadIO m)
               => Loggers m -> URI -> m (URI, Response B.ByteString)
downloadBinary (_, _errLog, _verbose) url =
    liftIO . browse $ do
        setAllowRedirects True
        -- setErrHandler errLog
        -- TODO find a way to use that
        -- setOutHandler verbose
        setOutHandler . const $ return ()
        setErrHandler . const $ return ()
        request $ defaultGETRequest_ url


