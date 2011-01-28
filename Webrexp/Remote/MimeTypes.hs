module Webrexp.Remote.MimeTypes ( isParseable) where

import qualified Data.Set as Set

isParseable :: String -> Bool
isParseable s = Set.member s parseableMimeTypes

parseableMimeTypes :: Set.Set String
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

