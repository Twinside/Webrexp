{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | This module store the interface between the evaluator
-- and the manipulated graph.
module Text.Webrexp.GraphWalker
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

    -- * Helper functions without monadic interface.
    , pureDescendants 
    , findNamedPure 
    , findFirstNamedPure
    ) where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Text.Webrexp.ProjectByteString as B

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
-- Minimal implementation : everything but deepValueOf.
class (GraphPath rezPath, Eq a)
        => GraphWalker a rezPath | a -> rezPath where
    -- | Get back an attribute of the node
    -- if it exists
    attribOf :: String -> a -> Maybe String

    -- | If the current node is named, return
    -- it's name, otherwise return Nothing.
    nameOf :: a -> Maybe String

    -- | Get all the children of the current
    -- node.
    childrenOf :: (MonadIO m) => a -> m [a]

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

    -- | Tell if the history associated is fixed or not.
    -- If the history is not fixed and can change (if you
    -- are querying the filesystem for example, it should
    -- return False)
    isHistoryMutable :: a -> Bool

    -- | Like value of, but force the node to collect the
    -- value of all it's children in the process.
    deepValueOf :: (MonadIO m, Functor m) => a -> m String
    deepValueOf node = (valueOf node ++) <$> childrenText
        where childrenText = concat <$> (childrenOf node >>= mapM deepValueOf)

-- | Return a list of all the "children"/linked node of a given node.
-- The given node is not included in the list.
-- A list of node with the taken path is returned.
descendants :: (MonadIO m, GraphWalker a r) => a -> m [(a, [(a, Int)])]
descendants node = findDescendants (node, [])
   where findDescendants (a, hist) = do
             children <- childrenOf a
             let lst = [ (child, (a,idx) : hist)
                         | (child, idx) <- zip children [0..]]
             xs <- mapM findDescendants lst
             return . concat $ lst : xs

-- | Given a tag and a name, retrieve
-- the first matching tags in the hierarchy.
-- It must return the list of ancestors permitting
-- the acess to the path used to find children
--
-- the returned list must contain : the node itself if
-- it match the name, and all the children containing the
-- good name.
findNamed :: (Functor m, MonadIO m, GraphWalker a r)
          => String -> a -> m [(a, [(a, Int)])]
findNamed name node = if nameOf node == Just name
                         then ((node, []) :) <$> validChildren
                         else validChildren
    where validChildren = filter (\(c,_) -> nameOf c == Just name)
                       <$> descendants node

-- | Return the first found node if any.
findFirstNamed :: (Functor m, MonadIO m, GraphWalker a r)
               => String -> [a] -> m (Maybe (a, [(a,Int)]))
findFirstNamed name lst = do
    nameList <- mapM (findNamed name) lst
    case concat nameList of
       [] -> return Nothing
       (x:_) -> return $ Just x

-- | like `descendants`, but without the monadic interface.
pureDescendants :: (a -> [a]) -> a -> [(a, [(a, Int)])]
pureDescendants pureChildren node = findDescendants (node, [])
   where findDescendants (a, hist) =
             let lst = [ (child, (a,idx) : hist)
                         | (child, idx) <- zip (pureChildren a) [0..]]
             in concat $ lst : map findDescendants lst

-- | Like `findNamed` but without the monadic interface.
findNamedPure :: (GraphWalker a r)
              => (a -> [a]) -> String -> a -> [(a, [(a,Int)])]
findNamedPure pureChildren name node = if nameOf node == Just name
                    then ((node, []) :) validChildren
                    else validChildren
    where validChildren = filter (\(c, _) -> nameOf c == Just name)
                        $ pureDescendants pureChildren node

-- | Like `findFirstNamed`, but without the monadic interface.
findFirstNamedPure :: (GraphWalker a r)
                   => (a -> [a]) -> String -> [a] -> Maybe (a, [(a,Int)])
findFirstNamedPure pureChildren name lst =
  case concat $ map (findNamedPure pureChildren name) lst of
     [] -> Nothing
     (x:_) -> Just x
