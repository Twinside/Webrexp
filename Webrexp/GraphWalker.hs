module Webrexp.GraphWalker
    ( GraphWalker(..)
    , NodePath
    , findNamed
    , findFirstNamed 
    ) where

import Webrexp.ResourcePath

-- | Represent the path used to find the node
-- from the starting point of the graph.
type NodePath a = [(a,Int)]

-- | The aim of this typeclass is to permit
-- the use of different html/xml parser if
-- if the first one is found to be bad. All
-- the logic should use this interface.
--
-- Minimal implementation : accessGraph, attribOf, childrenOf, valueOf
class GraphWalker a where
    -- | Get back an attribute of the node
    -- if it exists
    attribOf :: String -> a -> Maybe String

    -- | If the current node is named, return
    -- it's name, otherwise return Nothing.
    nameOf :: a -> Maybe String

    -- | Get all the children of the current
    -- node.
    childrenOf :: a -> [a]

    -- | Retrieve the value of the tag (textual)
    valueOf :: a -> String

    -- | The idea behind link following.
    -- The graph engine may have another name for the
    -- resource, so an updated name can be given.
    accessGraph :: ResourcePath -> IO (Maybe (ResourcePath, a))

-- | Given a tag and a name, retrieve
-- the first matching tags in the hierarchy.
-- It must return the list of ancestors permitting
-- the acess to the path used to find children
--
-- the returned list must contain :
--
findNamed :: (GraphWalker a)
          => String -> a -> [(a, [(a, Int)])]
findNamed name node = concat $
  thisNodeValid : map (findSubNamed . addHistory) (childrenOf node)
    where thisNodeValid = if nameOf node == Just name
                             then [(node,[])] else []
          addHistory a = (a, [])
          findSubNamed (a, hist) = concat $ 
            filter (\(c,_) -> nameOf c == Just name) lst : map findSubNamed lst
              where lst = [(child, (a,idx) : hist) |
                                (child, idx)<- zip (childrenOf a) [0..]]


findFirstNamed :: (GraphWalker a)
               => String -> [a] -> Maybe (a, [(a,Int)])
findFirstNamed name lst = if null results
                             then Nothing
                             else Just $ head results
    where results = concatMap (findNamed name) lst 

