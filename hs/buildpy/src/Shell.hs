module Shell where

import Control.Monad (forM_, when)
import System.Directory
import System.Directory.PathWalk (pathWalk)
import System.FilePath (joinPath)
import System.FilePattern (FilePattern, (?==))

import Log
import Process (cmd, run)
import Types (Name, Url)

makedir :: FilePath -> IO ()
makedir = createDirectoryIfMissing True

-- remove file or directory recursively
remove :: FilePath -> IO ()
remove = removePathForcibly

rename :: FilePath -> FilePath -> IO ()
rename = renamePath

move :: FilePath -> FilePath -> IO ()
move = rename

make :: [String] -> IO ()
make = run "make"

wget :: FilePath -> Url -> IO ()
wget dir url = cmd "wget" ["-P", dir, url] Nothing Nothing

curl :: FilePath -> Url -> IO ()
curl dir url = cmd "curl" ["-L", "--output-dir", dir, "-O", url] Nothing Nothing

tar :: FilePath -> FilePath -> IO ()
tar archive dir = cmd "tar" ["xvf", archive, "-C", dir] Nothing Nothing

zipLib :: FilePath -> FilePath -> IO ()
zipLib libpath zippath = cmd "zip" ["-r", zippath, "."] (Just libpath) Nothing

gitClone :: Url -> Name -> FilePath -> Bool -> IO ()
gitClone url branch dir recurse = do
    let args = ["clone", "--depth=1", "--branch", branch]
    let extras =
            if recurse
                then ["--recurse-submodules", "--shallow-submodules", url, dir]
                else [url, dir]
    cmd "git" (args ++ extras) Nothing Nothing

cmakeConfig ::
       FilePath -> FilePath -> [String] -> Maybe [(String, String)] -> IO ()
cmakeConfig src_dir build_dir opts =
    cmd "cmake" (["-S", src_dir, "-B", build_dir] ++ opts) Nothing

cmakeBuild :: String -> Bool -> IO ()
cmakeBuild build_dir release =
    cmd "cmake" (["--build", build_dir] ++ extras) Nothing Nothing
  where
    extras =
        if release
            then ["--config", "Release"]
            else []

cmakeInstall :: FilePath -> FilePath -> IO ()
cmakeInstall build_dir prefixPath =
    cmd "cmake" ["--install", build_dir, "--prefix", prefixPath] Nothing Nothing

globMatch :: [FilePattern] -> FilePath -> Bool
globMatch fs f = any (?== f) fs

-- walk :: (FilePath -> Bool) -> (FilePath -> IO ()) -> FilePath -> IO ()
-- walk match action root = do
--     pathWalk root $ \dir _ files -> do
--         forM_ files $ \file -> do
--             let target = joinPath [dir, file]
--             -- debug $ "searching: " ++ target
--             when (match file) $ do
--                 -- let target = joinPath [dir, file]
--                 info $ "found: " ++ target
--                 action target
walk :: (FilePath -> Bool) -> (FilePath -> IO ()) -> FilePath -> IO ()
walk match action root = do
    pathWalk root $ \dir subdirs _ -> do
        forM_ subdirs $ \subdir -> do
            let current_dir = joinPath [dir, subdir]
            -- info $ "searching: " ++ current_dir
            when (match current_dir) $ do
                -- info $ "rm: " ++ current_dir
                action current_dir
            -- debug $ "searching: " ++ target
        -- forM_ files $ \file -> do
        --     let current_file = joinPath [dir, file]
        --     when (match file) $ do
        --         -- info $ "rm: " ++ current_file
        --         action current_file

globRemove :: [FilePattern] -> FilePath -> IO ()
globRemove ps = walk (globMatch ps) action
        -- action = remove
  where
    action f = do
        exists <- doesPathExist f
        if exists
            then info $ "exists: " ++ f
            else warn $ "skipping: " ++ f
        -- action f = do 
        --     exists <- doesPathExist f
        --     if exists then removeDirectoryRecursive f else warn $ "skipping: " ++ f
        -- action f = warn $ "skipping: " ++ f
        -- action f = do 
        --     exists <- doesPathExist f
        --     if exists then removePathForcibly f else warn $ "skipping: " ++ f
