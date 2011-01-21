
module Webrexp.ResourcePath 
    ( ResourcePath (..)
    , toRezPath
    , (<//>)
    ) where

import Data.Maybe
import Network.URI
import System.FilePath

data ResourcePath =
      Local FilePath
    | Remote URI
    deriving (Eq, Show)

toRezPath :: String -> ResourcePath
toRezPath s = case parseURI s of
        Just u -> Remote u
        Nothing -> Local s

-- | Resource path combiner, similar to </> in use,
-- but also handle URI.
(<//>) :: ResourcePath -> ResourcePath -> ResourcePath
(<//>) (Local a) (Local b) = Local $ a </> b
(<//>) (Remote a) (Remote b) =
    case b `relativeTo` a of
         -- TODO : find another way for this
         Nothing -> Remote a
         Just c -> Remote c

(<//>) (Remote a) (Local b)
    | isRelativeReference b = case parseRelativeReference b of
        Just r -> Remote . fromJust $ r `relativeTo` a
        Nothing -> error "Not possible, checked before"

(<//>) _ _ = error "Mixing local/remote path"

