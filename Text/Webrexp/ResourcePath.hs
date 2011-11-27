-- | Module implementing plumbing to get a unified path locator,
-- handling URI & local path. Implement the 'GraphPath' and 'GraphWalker'
-- typeclass with 'HxtNode'
module Text.Webrexp.ResourcePath 
    ( ResourcePath (..)
    , rezPathToString
    , downloadBinary
    , accessResourcePath
    , rawLoadData
    ) where

import Text.Webrexp.GraphWalker
import Text.Webrexp.IOMock
import Control.Applicative
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

dumpResourcePath :: (Monad m, IOMockable m)
                 => Loggers m -> ResourcePath -> m ()
dumpResourcePath _ src@(Local source) = do
    maybeDirectory <- performIO getCurrentDirectory
    case maybeDirectory of
        Nothing -> return ()
        Just cwd -> do
            _ <- performIO . copyFile source $ cwd </> extractFileName src
            return ()

dumpResourcePath loggers@(logger,_,_) p@(Remote a) = do
  (_, rsp) <- downloadBinary loggers a
  let rawFilename = extractFileName p
      filename = case retrieveHeaders HdrContentType rsp of
         [] -> rawFilename 
         (hdr:_) -> addContentTypeExtension
                            (hdrValue hdr) rawFilename
                 

  logger $ "Downloading '" ++ show a ++ "' in '" ++ filename
  _ <- performIO . B.writeFile filename $ rspBody rsp
  return ()

accessResourcePath :: (Monad m, IOMockable m, Functor m)
                   => Loggers m -> ResourcePath -> m (AccessResult a ResourcePath)
accessResourcePath loggers rpath = maybe AccessError (DataBlob rpath)
                                <$> rawLoadData loggers rpath

-- | Extract a blob of data from a resourcepath and return
-- the result.
rawLoadData :: (Monad m, IOMockable m)
            => Loggers m -> ResourcePath -> m (Maybe B.ByteString)
rawLoadData (logger, _errLog, _verbose) (Local s) = do
    logger $ "Opening file : '" ++ s ++ "'"
    realFile <- performIO $ doesFileExist s
    case realFile of
      Just True -> performIO (B.readFile s)
      _         -> return Nothing

rawLoadData loggers@(logger, _, _verbose) (Remote uri) = do
  logger $ "Downloading URL : '" ++ show uri ++ "'"
  (_u, rsp) <- downloadBinary loggers uri
  if rspBody rsp == B.empty 
  	then return Nothing
  	else return . Just $ rspBody rsp


-- | Helper function to grab a resource on internet and returning
-- it's binary representation, and it's real place if any.
downloadBinary :: (Monad m, IOMockable m)
               => Loggers m -> URI -> m (URI, Response B.ByteString)
downloadBinary (_, _errLog, _verbose) url = do
    rez <- performIO . browse $ do
        setAllowRedirects True
        -- setErrHandler errLog
        -- TODO find a way to use that
        -- setOutHandler verbose
        setOutHandler . const $ return ()
        setErrHandler . const $ return ()
        request $ defaultGETRequest_ url
    case rez of
        Nothing -> return (url, Response { rspCode = (4, 0, 4)
                                         , rspReason = "Not allowed" 
                                         , rspHeaders = []
                                         , rspBody = B.empty })
        Just r -> return r


