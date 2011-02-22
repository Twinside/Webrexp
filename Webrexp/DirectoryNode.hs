{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Webrexp.DirectoryNode( DirectoryNode 
                            , toDirectoryNode
                            , currentDirectoryNode 
                            ) where

import Control.Exception
import Control.Monad.IO.Class
import System.Directory
import System.FilePath

import Webrexp.GraphWalker
import Webrexp.ResourcePath
import Webrexp.UnionNode
import Webrexp.WebContext

type FileName = String

-- | Type representing a local folder directory as a node
-- (and not as a path)
data DirectoryNode = 
      Directory FilePath FileName
    | File FilePath FileName
    deriving (Eq)

extractPath :: DirectoryNode -> FilePath
extractPath (Directory a _) = a
extractPath (File a _) = a

buildParentList :: FilePath -> [DirectoryNode]
buildParentList path = map (uncurry Directory) nameFullName
   where directoryList = splitDirectories path
         -- First the name of the folder, followed by the whole path
         nameFullName = zip directoryList $ scanl1 (</>) directoryList


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
            , this = Directory path fname
            , rootRef = Local . extractPath $ head parentPath
            }
         (True, _) -> return . Just $ NodeContext
            { parents = MutableHistory $ reverse parentPath
            , this = File path fname
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
    dummyElem = undefined

    isResourceParseable _ (Local _) _ = True
    isResourceParseable _ _ _ = False

    parseResource _ _ _ = Nothing

instance GraphWalker DirectoryNode ResourcePath where
    -- For now, no file attribute, in the future, might
    -- be interesting to map size & other information
    -- here.
    attribOf _ _ = Nothing

    nameOf (Directory _ name) = Just name
    nameOf (File _ name) = Just name

    valueOf (File fpath _) = fpath
    valueOf (Directory fpath _) = fpath
    
    indirectLinks (File fpath _) = [Local fpath]
    indirectLinks (Directory _ _) = []

    accessGraph _ _ = return AccessError

    childrenOf (File _ _) = return []
    childrenOf (Directory path _) = liftIO $ listDirectory path


listDirectory :: FilePath -> IO [DirectoryNode]
listDirectory fpath = do
    content <- try $ getDirectoryContents fpath
    case content of
       Left (_ :: IOError) -> return []
       Right lst ->
         mapM (\path -> do
            isDir <- doesDirectoryExist path
            let f = if isDir then Directory else File
            return . f path $ takeFileName path) lst

