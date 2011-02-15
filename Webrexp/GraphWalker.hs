{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | This module store the interface between the evaluator
-- and the manipulated graph.
module Webrexp.GraphWalker
    ( 
    -- * Classes
      GraphWalker(..)
    , GraphPath(..)

    -- * Commodity types
    , AccessResult(..)
    , Logger
    , Loggers
    , NodePath

    -- * Helper functions.
    , descendants 
    , findNamed
    , findFirstNamed 
    ) where

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B

-- | Represent the path used to find the node
-- from the starting point of the graph.
type NodePath a = [(a,Int)]

-- | Type used to propagate different logging
-- level across the software.
type Logger = String -> IO ()

-- | Normal/Err/verbose loggers.
type Loggers = (Logger, Logger, Logger)

-- | Represent indirect links or links which
-- necessitate the use of the IO monad to walk
-- around the graph.
class (Show a) => GraphPath a where
    -- | Combine two path togethers, you can think
    -- of the </> operator for an equivalence.
    (<//>) :: a -> a -> a

    -- | conversion to be used to import path
    -- from attributes/document (not really
    -- well specified).
    importPath :: String -> Maybe a

    -- | Move semantic, try to dump the pointed
    -- resource to the current folder.
    dumpDataAtPath :: (Monad m, MonadIO m)
                   => Loggers -> a
                   -> m ()

    -- | Given a graphpath, transform it to
    -- a filepath which can be used to store
    -- a node.
    localizePath :: a -> FilePath

-- | Result of indirect access demand.
data AccessResult a rezPath =
    -- | We got a result and parsed it, maybe
    -- it has changed of location, so we give
    -- back the location
      Result rezPath a
    -- | We got something, but we can't interpret
    -- it, so we return a binary blob.
    | DataBlob rezPath B.ByteString
    -- | Cannot access the resource.
    | AccessError

-- | The aim of this typeclass is to permit
-- the use of different html/xml parser if
-- if the first one is found to be bad. All
-- the logic should use this interface.
--
-- Minimal implementation : everything.
class (Show a, GraphPath rezPath)
        => GraphWalker a rezPath | a -> rezPath where
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

    -- | Retrieve all the indirectly linked content
    -- of a node, can be used for element like an
    -- HTML link or an linked image/obj
    indirectLinks :: a -> [rezPath]

    -- | The idea behind link following.
    -- The graph engine may have another name for the
    -- resource, so an updated name can be given.
    -- The given function is there to log information,
    -- the second is to log errors
    accessGraph :: (MonadIO m)
                => Loggers -> rezPath -> m (AccessResult a rezPath)

-- | Return a list of all the "children"/linked node of a given node.
-- The given node is not included in the list.
-- A list of node with the taken path is returned.
descendants :: (GraphWalker a r) => a -> [(a, [(a, Int)])]
descendants node = findDescendants (node, [])
   where findDescendants (a, hist) = concat $ lst : map findDescendants lst
              where lst = [(child, (a,idx) : hist) |
                                (child, idx) <- zip (childrenOf a) [0..]]

-- | Given a tag and a name, retrieve
-- the first matching tags in the hierarchy.
-- It must return the list of ancestors permitting
-- the acess to the path used to find children
--
-- the returned list must contain : the node itself if
-- it match the name, and all the children containing the
-- good name.
findNamed :: (GraphWalker a r)
          => String -> a -> [(a, [(a, Int)])]
findNamed name node = if nameOf node == Just name
                         then (node, []) : validChildren
                         else validChildren
    where validChildren = filter (\(c,_) -> nameOf c == Just name)
                        $ descendants node

-- | Return the first found node if any.
findFirstNamed :: (GraphWalker a r)
               => String -> [a] -> Maybe (a, [(a,Int)])
findFirstNamed name lst = case results of
                             [] -> Nothing
                             (x:_) -> Just x

    where results = concatMap (findNamed name) lst 

