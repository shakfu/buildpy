module Main where



import System.Directory
import System.FilePath
import System.FilePattern (FilePattern, (?==))
import System.Directory.PathWalk ( pathWalk )
import Control.Monad ( when, forM_ )

import System.Environment (getArgs)


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

walkdirs :: (FilePath -> Bool) -> (FilePath -> IO ()) -> FilePath -> IO ()
walkdirs match action root = do
    pathWalk root $ \dir subdirs _ -> do
        forM_ subdirs $ \subdir -> do
            let current_dir = joinPath [dir, subdir]
            when (match current_dir) $ do
                -- putStrLn $ "rm: " ++ current_dir
                action current_dir

walkfiles :: (FilePath -> Bool) -> (FilePath -> IO ()) -> FilePath -> IO ()
walkfiles match action root = do
    pathWalk root $ \dir _ files -> do
        forM_ files $ \file -> do
            let current_file = joinPath [dir, file]
            when (match file) $ do
                putStrLn $ "rm: " ++ current_file
                action current_file

globDirRemove :: [FilePattern] -> FilePath -> IO ()
globDirRemove ps = walkdirs (globMatch ps) action
    where
        action f = do 
            exists <- doesPathExist f
            if exists then removePathForcibly f else putStrLn $ "skipping: " ++ f

globFileRemove :: [FilePattern] -> FilePath -> IO ()
globFileRemove ps = walkfiles (globMatch ps) action
    where
        action f = do 
            exists <- doesPathExist f
            if exists then removePathForcibly f else putStrLn $ "skipping: " ++ f


filepatterns :: [FilePattern]
filepatterns = ["*.pyc"]

dirpatterns :: [FilePattern]
dirpatterns = ["**/__pycache__"]


-- main :: IO ()
-- -- main = globRemove filepatterns "./python3.12"
-- main = globDirRemove dirpatterns "./python3.12"


-- module Main (main) where


main :: IO ()
main = do
  rawArgs <- getArgs
  let args = if rawArgs == [] then ["."] else rawArgs
  forM_ args $ \arg -> do
    pathWalk arg $ \root dirs files -> do
      putStrLn root
      putStrLn $ "  dirs: " ++ show dirs
      putStrLn $ "  files: " ++ show files