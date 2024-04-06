module Main where



import System.Directory
import System.FilePath
import System.FilePattern
import System.Directory.PathWalk 
-- import Control.Monad ( when, forM_ )

import System.Environment

import Control.Monad
import System.Posix.Files



globMatch :: [FilePattern] -> FilePath -> Bool
globMatch fs f = any (?== f) fs

-- walk :: (FilePath -> Bool) -> (FilePath -> IO ()) -> FilePath -> IO ()
-- walk match action root = do
--     pathWalk root $ \dir _ files -> do
--         forM_ files $ \file -> do
--             let target = joinPath [dir, file]
--             when (match file) $ do
--                 let target = joinPath [dir, file]
--                 putStrLn $ "found: " ++ target
--                 action target

-- walkdirs :: (FilePath -> Bool) -> (FilePath -> IO ()) -> FilePath -> IO ()
-- walkdirs match action root = do
--     pathWalk root $ \dir subdirs _ -> do
--         forM_ subdirs $ \subdir -> do
--             let current_dir = joinPath [dir, subdir]
--             when (match current_dir) $ do
--                 -- putStrLn $ "rm: " ++ current_dir
--                 action current_dir
--                 return ()

-- walkdirs :: (FilePath -> Bool) -> (FilePath -> IO ()) -> FilePath -> IO ()
-- walkdirs match action root = do
--     pathWalk root $ \dir subdirs _ -> do
--         forM_ subdirs $ \subdir -> do
--             let current_dir = joinPath [dir, subdir]
--             when (match current_dir) $ do
--                 -- putStrLn $ "rm: " ++ current_dir
--                 action current_dir

-- walkfiles :: (FilePath -> Bool) -> (FilePath -> IO ()) -> FilePath -> IO ()
-- walkfiles match action root = do
--     pathWalk root $ \dir _ files -> do
--         forM_ files $ \file -> do
--             let current_file = joinPath [dir, file]
--             when (match file) $ do
--                 putStrLn $ "rm: " ++ current_file
--                 action current_file

-- globDirRemove :: [FilePattern] -> FilePath -> IO ()
-- globDirRemove ps = walkdirs (globMatch ps) action
--     where
--         action f = do 
--             exists <- doesPathExist f
--             if exists then removePathForcibly f else putStrLn $ "skipping: " ++ f

-- globFileRemove :: [FilePattern] -> FilePath -> IO ()
-- globFileRemove ps = walkfiles (globMatch ps) action
--     where
--         action f = do 
--             exists <- doesPathExist f
--             if exists then removePathForcibly f else putStrLn $ "skipping: " ++ f


filepatterns :: [FilePattern]
-- filepatterns = ["**/*.pyc"]
filepatterns = ["*.pyc"]

dirpatterns :: [FilePattern]
dirpatterns = ["**/__pycache__"]


-- main :: IO ()
-- -- main = globRemove filepatterns "./python3.12"
-- main = globDirRemove dirpatterns "./python3.12"


-- module Main (main) where


-- -- | Traverse from 'top' directory and return all the files by
-- -- filtering out the 'exclude' predicate.
-- traverseDir :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
-- traverseDir top exclude = do
--   ds <- getDirectoryContents top
--   paths <- forM (filter (not.exclude) ds) $ \d -> do
--     let path = top </> d
--     s <- getFileStatus path
--     if isDirectory s
--       then traverseDir path exclude
--       else return [path]
--   return (concat paths)


-- | Traverse from 'top' directory and return all the files by
-- filtering out the 'exclude' predicate.
-- traverseDir :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
-- traverseDir top match = do
--   ds <- getDirectoryContents top
--   -- putStrLn $ "dirs: " ++ show ds
--   paths <- forM (filter match ds) $ \d -> do
--     let path = top </> d
--     putStrLn $ "path: " ++ show path
--     s <- getFileStatus path
--     if isDirectory s
--       then traverseDir path match
--       else return [path]
--   return (concat paths)


-- main :: IO ()
-- main = do
    -- -- fs <- traverseDir "python3.12" (globMatch ["*.py"])
    -- print fs


-- λ> filter (globMatch filepatterns) ["hello.pyc", "./sub/by.pyc", "b.py"]
-- ["hello.pyc","./sub/by.pyc"]

-- main :: IO ()
-- main = do
--   rawArgs <- getArgs
--   let args = if rawArgs == [] then ["."] else rawArgs
--   forM_ args $ \arg -> do
--     pathWalk arg $ \root dirs files -> do
--       putStrLn root
--       putStrLn $ "  dirs: " ++ show dirs
--       putStrLn $ "  files: " ++ show files


-- walk :: (FilePath -> Bool) -> (FilePath -> IO ()) -> FilePath -> IO ()
-- walk match action root = do
--     pathWalk root $ \dir subdirs files -> do
--         forM_ files $ \file -> do
--             let target = joinPath [dir, file]
--             when (match file) $ do
--                 let target = joinPath [dir, file]
--                 putStrLn $ "found: " ++ target
--                 action target



-- main :: IO ()
-- main = walk (globMatch filepatterns) removePathForcibly "python3.12"

-- main :: IO ()
-- main = walkdirs (globMatch dirpatterns) removePathForcibly "python3.12"

getAllFiles :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
getAllFiles match path = do
    isFile <- doesFileExist path
    if and [isFile, match path]
        then return [path] -- if this is a file, return it
        else do
            -- if it's not a file, we assume it to be a directory
            dirContents <- listDirectory path
            -- run this function recursively on all the children
            -- and accumulate the results
            fmap concat $ mapM ((getAllFiles match) . (path </>)) dirContents

main :: IO ()
main = do 
    fs <- getAllFiles (globMatch filepatterns) "python3.12"
    print fs


