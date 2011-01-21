module Webrexp.GraphWalker
    ( GraphWalker(..)
    , findNamed
    ) where

import Webrexp.ResourcePath

-- | The aim of this typeclass is to permit
-- the use of different html/xml parser if
-- if the first one is found to be bad. All
-- the logic should use this interface.
--
-- Minimal implementation : accessGraph, attribOf, childrenOf, valueOf
class GraphWalker a where
    -- | Get back an attribute of the node
    -- if it exists
    attribOf :: a -> String -> Maybe String

    -- | Get all the children of the current
    -- node.
    childrenOf :: a -> [a]

    -- | Retrieve the value of the tag (textual)
    valueOf :: a -> String

    -- | The idea behind link following
    accessGraph :: ResourcePath -> IO (Maybe a)

-- | Given a tag and a name, retrieve
-- the first matching tags in the hierarchy.
-- It must return the list of ancestors permitting
-- the acess to the path used to find children
--
-- the returned list must contain :
--
findNamed :: (GraphWalker a)
          => a -> String -> [(a, [(a, Int)])]
findNamed _ _ = []

