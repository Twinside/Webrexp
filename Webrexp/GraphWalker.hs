module Webrexp.GraphWalker
    ( GraphWalker(..)
    ) where

-- | The aim of this typeclass is to permit
-- the use of different html/xml parser if
-- if the first one is found to be bad. All
-- the logic should use this interface.
class GraphWalker a where
    -- | Get back an attribute of the node
    -- if it exists
    attribOf :: a -> String -> Maybe String

    -- | Get all the children of the current
    -- node.
    childrenOf :: a -> [a]

    -- | Given a tag and a name, retrieve
    -- the first matching tags in the hierarchy.
    findNamed :: a -> String -> [a]

    -- | Retrieve the value of the tag (textual)
    valueOf :: a -> String

