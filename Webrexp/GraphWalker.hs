module Webrexp.GraphWalker
    ( GraphWalker(..)
    , Logger
    , NodePath
    , findNamed
    , findFirstNamed 
    ) where

import Control.Monad.IO.Class
import Webrexp.ResourcePath

-- | Represent the path used to find the node
-- from the starting point of the graph.
type NodePath a = [(a,Int)]

type Logger = String -> IO ()

-- | The aim of this typeclass is to permit
-- the use of different html/xml parser if
-- if the first one is found to be bad. All
-- the logic should use this interface.
--
-- Minimal implementation : accessGraph, attribOf, childrenOf, valueOf
class (Show a) => GraphWalker a where
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
    -- The given function is there to log information,
    -- the second is to log errors
    accessGraph :: (MonadIO m)
                => Logger -> Logger -> Logger -> ResourcePath 
                -> m (Maybe (ResourcePath, a))

-- | Given a tag and a name, retrieve
-- the first matching tags in the hierarchy.
-- It must return the list of ancestors permitting
-- the acess to the path used to find children
--
-- the returned list must contain : the node itself if
-- it match the name, and all the children containing the
-- good name.
findNamed :: (GraphWalker a)
          => String -> a -> [(a, [(a, Int)])]
findNamed name node = thisNodeValid ++ findSubNamed (node, [])
    where thisNodeValid = if nameOf node == Just name
                                then [(node,[])] else []
          findSubNamed (a, hist) = concat $
            filter (\(c,_) -> nameOf c == Just name) lst : map findSubNamed lst
              where lst = [(child, (a,idx) : hist) |
                                (child, idx)<- zip (childrenOf a) [0..]]

-- | Return the first found node if any.
findFirstNamed :: (GraphWalker a)
               => String -> [a] -> Maybe (a, [(a,Int)])
findFirstNamed name lst = if null results
                             then Nothing
                             else Just $ head results
    where results = concatMap (findNamed name) lst 

