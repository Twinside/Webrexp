-- | This module implement helper functions to determine if
-- downloaded URI can be parsed as HTML/XML. Everything
-- is based on MIME-TYPE, or file extension for local content.
module Webrexp.Remote.MimeTypes ( -- * Types
                                  ContentType
                                , ParseableType(..)
                                  -- * Functions
                                , getParserForMimeType 
                                , getParseKind
                                , addContentTypeExtension
                                ) where

import System.FilePath
import qualified Data.Map as Map

-- | Describe different kind of content parser usable
data ParseableType =
      -- | Indicate a parser which must be tolerant enough
      -- to parse HTML
      ParseableHTML
      -- | You can go ahead and use a rather strict parser.
    | ParseableXML
      -- | Do what you want with it for now.
    | ParseableJson
    deriving Eq

-- | Type alias to ease documentation.
type ContentType = String

-- | Content-type field of HTTP header
findContentTypeOf :: ContentType -> ContentType
findContentTypeOf = fst . break (';' ==)
             
-- | Associate extension to parser, used for local file type
-- recognition.
fileExtension :: Map.Map String ParseableType
fileExtension = Map.fromList
    [('.' : ext, v) | (ext, Just v) <- Map.elems mimeExtension]

-- | Given a MimeType, return the kind of parser to use
-- for the given data.
getParserForMimeType :: ContentType -> Maybe ParseableType
getParserForMimeType ctt = Map.lookup (findContentTypeOf ctt) mimeExtension
                         >>= snd

-- | Given a file name, return a ParseableType, explaining
-- the kind of parser to use on the given content.
getParseKind :: FilePath -> Maybe ParseableType
getParseKind f = case Map.lookup (takeExtension f) fileExtension of
        Nothing -> Nothing
        Just s  -> Just s

-- | Given a content type, the same as 'isParseable', and
-- a filepath, we add a type extension if the the filepath
-- don't have any.
--
-- The intent is to add a valid extension given a valid
-- MIME-TYPE, to get correct OS behaviour.
addContentTypeExtension :: ContentType -> FilePath -> FilePath
addContentTypeExtension ctype path
    | hasExtension path = path
    | otherwise = case Map.lookup (findContentTypeOf ctype) 
                                  mimeExtension of
        Nothing -> path
        Just (s,_) -> path <.> s

-- | Mimetype/extension association.
mimeExtension :: Map.Map String (String, Maybe ParseableType)
mimeExtension = Map.fromList
    [("application/atom+xml", ("xml", Just ParseableXML))
    ,("application/json", ("json", Just ParseableJson))
    ,("application/javascript", ("js", Nothing))
    ,("application/octet-stream", ("dat", Nothing))
    ,("application/ogg", ("ogg", Nothing))
    ,("application/pdf", ("pdf", Nothing))
    ,("application/postscript", ("ps", Nothing))
    ,("application/soap+xml", ("xml", Just ParseableXML))
    ,("application/xhtml+xml", ("xhtml", Just ParseableHTML))
    ,("application/xml-dtd", ("dtd", Nothing))
    ,("application/xml", ("xml", Just ParseableXML))
    ,("application/rss+xml", ("rss", Just ParseableXML))
    ,("application/xslt+xml", ("xslt", Just ParseableXML))
    ,("application/mathml+xml", ("mathml", Just ParseableXML))
    ,("application/zip", ("zip", Nothing))
    ,("audio/mp4", ("mp4", Nothing))
    ,("audio/mpeg", ("mpg", Nothing))
    ,("audio/ogg", ("ogg", Nothing))
    ,("audio/vorbis", ("ogg", Nothing))
    ,("audio/x-ms-wma", ("wma", Nothing))
    ,("audio/x-ms-wax", ("", Nothing))
    ,("audio/vnd.wave", ("wav", Nothing))
    ,("image/gif", ("gif", Nothing))
    ,("image/jpeg", ("jpg", Nothing))
    ,("image/png", ("png", Nothing))
    ,("image/svg+xml", ("svg", Just ParseableXML))
    ,("image/tiff", ("tiff", Nothing))
    ,("image/vnd.microsoft.icon", ("icon", Nothing))
    ,("text/cmd", ("cmd", Nothing))
    ,("text/css", ("css", Nothing))
    ,("text/csv", ("csv", Nothing))
    ,("text/html", ("html", Just ParseableHTML))
    ,("text/javascript", ("js", Nothing))
    ,("text/plain", ("txt", Nothing))
    ,("text/xml", ("xml", Just ParseableXML))
    ,("video/mpeg", ("mpg", Nothing))
    ,("video/mp4", ("mp4", Nothing))
    ,("video/ogg", ("ogm", Nothing))
    ,("video/quicktime", ("mov", Nothing))
    ,("video/webm", ("webm", Nothing))
    ,("video/x-ms-wmv", ("wmv", Nothing))
    ,("application/vnd.oasis.opendocument.text", ("odt", Nothing))
    ,("application/vnd.oasis.opendocument.spreadsheet", ("ods", Nothing))
    ,("application/vnd.oasis.opendocument.presentation", ("odp", Nothing))
    ,("application/vnd.oasis.opendocument.graphics", ("odg", Nothing))
    ,("application/vnd.ms-excel", ("xls", Nothing))
    ,("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", ("xslx", Nothing))
    ,("application/vnd.ms-powerpoint", ("ppt", Nothing))
    ,("application/vnd.openxmlformats-officedocument.presentationml.presentation", ("pptx", Nothing))
    ,("application/msword", ("doc", Nothing))
    ,("application/vnd.openxmlformats-officedocument.wordprocessingml.document", ("docx", Nothing))
    ,("application/vnd.mozilla.xul+xml", ("xml", Just ParseableXML))
    ,("application/x-dvi", ("dvi", Nothing))
    ,("application/x-latex", ("tex", Nothing))
    ,("application/x-font-ttf", ("ttf", Nothing))
    ,("application/x-shockwave-flash", ("swf", Nothing))
    ,("application/x-rar-compressed", ("rar", Nothing))
    ,("application/x-tar", ("tar", Nothing))
    ]
    
