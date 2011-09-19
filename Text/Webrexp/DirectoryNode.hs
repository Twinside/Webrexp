{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Webrexp.DirectoryNode( DirectoryNode
                            , toDirectoryNode
                            , currentDirectoryNode
                            ) where

import Control.Exception
import Data.Maybe( fromMaybe )
import System.Directory
import System.FilePath

import Text.Webrexp.IOMock
import Text.Webrexp.GraphWalker
import Text.Webrexp.ResourcePath
import Text.Webrexp.UnionNode
import Text.Webrexp.WebContext

type FileName = String

-- | Type introduced to avoid stupid positional
-- errors in the 'DirectoryNode' type.
newtype FullPath = FullPath String
    deriving (Eq, Show)

-- | Type representing a local folder directory as a node
-- (and not as a path)
data DirectoryNode =
      Directory FullPath FileName
    | File FullPath FileName
    deriving (Eq, Show)

extractPath :: DirectoryNode -> FilePath
extractPath (Directory (FullPath a) _) = a
extractPath (File (FullPath a) _) = a

buildParentList :: FilePath -> [DirectoryNode]
buildParentList path = map directoryze nameFullName
   where directoryList = splitDirectories path
         -- First the name of the folder, followed by the whole path
         nameFullName = zip directoryList $ scanl1 (</>) directoryList

         directoryze (name, whole) =
             Directory (FullPath whole) name


-- | Transform a filepath into a valid directory node
-- if the path is valid in the current system.
toDirectoryNode :: FilePath -> IO (Maybe (NodeContext DirectoryNode ResourcePath))
toDirectoryNode path = do
    existing <- doesFileExist path
    dirExist <- doesDirectoryExist path
    let (wholePath, fname) = splitFileName path
        parentPath = buildParentList wholePath
    case (existing, dirExist) of
         (_, True) -> return . Just $ NodeContext
            { parents = MutableHistory $ reverse parentPath
            , this = Directory (FullPath path) fname
            , rootRef = Local . extractPath $ head parentPath
            }
         (True, _) -> return . Just $ NodeContext
            { parents = MutableHistory $ reverse parentPath
            , this = File (FullPath path) fname
            , rootRef = Local . extractPath $ head parentPath
            }
         _ -> return Nothing

-- | Create a node rooted in the current directory.
currentDirectoryNode :: IO (NodeContext DirectoryNode ResourcePath)
currentDirectoryNode = do
    cwd <- getCurrentDirectory
    node <- toDirectoryNode cwd
    case node of
        Nothing -> error "currentDirectoryNode : node is not a directory/file o_O"
        Just s -> return s

-- | The problem of this instance is the fact that
-- it's a "sink" instance, it accepts everything.
instance PartialGraph DirectoryNode ResourcePath where
    isResourceParseable _ (Local _) _ = True
    isResourceParseable _ _ _ = False

    parseResource _ _ _ _ = return Nothing

instance GraphWalker DirectoryNode ResourcePath where
    -- For now, no file attribute, in the future, might
    -- be interesting to map size & other information
    -- here.
    attribOf _ _ = Nothing

    nameOf (Directory _ name) = Just name
    nameOf (File _ name) = Just name

    valueOf (File (FullPath fpath) _) = fpath
    valueOf (Directory (FullPath fpath) _) = fpath

    indirectLinks (File _ _) = []
    indirectLinks (Directory _ _) = []

    accessGraph _ _ = return AccessError

    isHistoryMutable _ = True

    childrenOf (File _ _) = return []
    childrenOf (Directory (FullPath path) _) =
         performIO (listDirectory path) >>= return . fromMaybe []



listDirectory :: FilePath -> IO [DirectoryNode]
listDirectory fpath = do
    content <- try $ getDirectoryContents fpath
    case content of
       Left (_ :: IOError) -> return []
       Right lst ->
         mapM (\path -> do
            let wholePath = fpath </> path
            isDir <- doesDirectoryExist wholePath
            let f = if isDir then Directory else File
            return $ f (FullPath wholePath) path)

              $ filter (\a -> a `notElem` [".", ".."]) lst

