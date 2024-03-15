module Shell where

import System.Directory
import System.FilePattern

import Process (cmd, run)

makedirs :: FilePath -> IO ()
makedirs = createDirectoryIfMissing True

-- createDirectory :: FilePath -> IO ()
-- remove file or directory recursively
remove :: FilePath -> IO ()
remove = removePathForcibly

rename :: FilePath -> FilePath -> IO ()
rename = renamePath

make :: [String] -> IO ()
make = run "make"

wget :: String -> String -> IO ()
wget dir url = cmd "wget" ["-P", dir, url] Nothing Nothing

curl :: String -> String -> IO ()
curl dir url = cmd "curl" ["-L", "--output-dir", dir, "-O", url] Nothing Nothing

tar :: String -> String -> IO ()
tar archive dir = cmd "tar" ["xvf", archive, "-C", dir] Nothing Nothing

gitClone :: String -> String -> String -> Bool -> IO ()
gitClone url branch dir recurse = do
    let args = ["clone", "--depth=1", "--branch", branch]
    let extras =
            if recurse
                then ["--recurse-submodules", "--shallow-submodules", url, dir]
                else [url, dir]
    cmd "git" (args ++ extras) Nothing Nothing

cmakeConfig :: String -> String -> [String] -> IO ()
cmakeConfig src_dir build_dir opts =
    cmd "cmake" (["-S", src_dir, "-B", build_dir] ++ opts) Nothing Nothing

cmakeBuild :: String -> Bool -> IO ()
cmakeBuild build_dir release =
    cmd "cmake" (["--build", build_dir] ++ extras) Nothing Nothing
  where
    extras =
        if release
            then ["--config", "Release"]
            else []

cmakeInstall :: String -> String -> IO ()
cmakeInstall build_dir prefix =
    cmd "cmake" ["--install", build_dir, "--prefix", prefix] Nothing Nothing

isGlobMatch :: FilePath -> [String] -> Bool
isGlobMatch f patterns = any (== True) $ map (flip (?==) f) patterns
