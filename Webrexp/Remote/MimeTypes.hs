-- | This module implement helper functions to determine if
-- downloaded URI can be parsed as HTML/XML. Everything
-- is based on MIME-TYPE.
module Webrexp.Remote.MimeTypes ( ContentType
                                , isParseable
                                , addContentTypeExtension 
                                ) where

import System.FilePath
import qualified Data.Set as Set
import qualified Data.Map as Map

-- | Type alias to ease documentation.
type ContentType = String

findContentTypeOf :: ContentType -> ContentType
findContentTypeOf = fst . break (';' ==)

-- | Take a Content-Type HTTP field value
-- and tell if we can parse it. You don't
-- need to take care about splitting it.
--
-- For exemple \"text/html; charset=UTF-8\"
-- is an acceptable input, we will extract
-- \"text/html\" ourselves.
isParseable :: ContentType -> Bool
isParseable s = Set.member (findContentTypeOf s)
                            parseableMimeTypes
             

-- | Mime Types who can be parsed.
parseableMimeTypes :: Set.Set ContentType
parseableMimeTypes = Set.fromList
    [ "application/atom+xml"
    , "application/soap+xml"
    , "application/xhtml+xml"
    , "application/xml"
    , "application/rss+xml"
    , "application/xslt+xml"
    , "application/mathml+xml"
    , "image/svg+xml"
    , "text/html"
    , "text/xml"
    ]

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
        Just s -> path <.> s

-- | Mimetype/extension association.
mimeExtension :: Map.Map String String
mimeExtension = Map.fromList
    [("application/atom+xml", "xml")
    ,("application/json", "json")
    ,("application/javascript", "js")
    ,("application/octet-stream", "dat")
    ,("application/ogg", "ogg")
    ,("application/pdf", "pdf")
    ,("application/postscript", "ps")
    ,("application/soap+xml", "xml")
    ,("application/xhtml+xml", "html")
    ,("application/xml-dtd", "dtd")
    ,("application/zip", "zip")
    ,("audio/mp4", "mp4")
    ,("audio/mpeg", "mpg")
    ,("audio/ogg", "ogg")
    ,("audio/vorbis", "ogg")
    ,("audio/x-ms-wma", "wma")
    ,("audio/x-ms-wax", "")
    ,("audio/vnd.wave", "wav")
    ,("image/gif", "gif")
    ,("image/jpeg", "jpg")
    ,("image/png", "png")
    ,("image/svg+xml", "svg")
    ,("image/tiff", "tiff")
    ,("image/vnd.microsoft.icon", "icon")
    ,("text/cmd", "cmd")
    ,("text/css", "css")
    ,("text/csv", "csv")
    ,("text/html", "html")
    ,("text/javascript", "js")
    ,("text/plain", "txt")
    ,("text/xml", "xml")
    ,("video/mpeg", "mpg")
    ,("video/mp4", "mp4")
    ,("video/ogg", "ogm")
    ,("video/quicktime", "mov")
    ,("video/webm", "webm")
    ,("video/x-ms-wmv", "wmv")
    ,("application/vnd.oasis.opendocument.text", "odt")
    ,("application/vnd.oasis.opendocument.spreadsheet", "ods")
    ,("application/vnd.oasis.opendocument.presentation", "odp")
    ,("application/vnd.oasis.opendocument.graphics", "odg")
    ,("application/vnd.ms-excel", "xls")
    ,("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", "xslx")
    ,("application/vnd.ms-powerpoint", "ppt")
    ,("application/vnd.openxmlformats-officedocument.presentationml.presentation", "pptx")
    ,("application/msword", "doc")
    ,("application/vnd.openxmlformats-officedocument.wordprocessingml.document", "docx")
    ,("application/vnd.mozilla.xul+xml", "xml")
    ,("application/x-dvi", "dvi")
    ,("application/x-latex", "tex")
    ,("application/x-font-ttf", "ttf")
    ,("application/x-shockwave-flash", "swf")
    ,("application/x-rar-compressed", "rar")
    ,("application/x-tar", "tar")
    ]
    
