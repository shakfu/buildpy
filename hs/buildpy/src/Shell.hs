module Shell where

import System.Directory
    ( createDirectoryIfMissing, removePathForcibly, renamePath )
import System.FilePattern ( (?==), FilePattern )

import Process (cmd, run)


makedir :: FilePath -> IO ()
makedir = createDirectoryIfMissing True

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

ziplib :: String -> String -> IO ()
ziplib libpath zippath = cmd "zip" ["-r", zippath, "."] (Just libpath) Nothing

gitClone :: String -> String -> String -> Bool -> IO ()
gitClone url branch dir recurse = do
  let args = ["clone", "--depth=1", "--branch", branch]
  let extras =
        if recurse
          then ["--recurse-submodules", "--shallow-submodules", url, dir]
          else [url, dir]
  cmd "git" (args ++ extras) Nothing Nothing

cmakeConfig :: String -> String -> [String] -> Maybe [(String, String)] -> IO ()
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

cmakeInstall :: String -> String -> IO ()
cmakeInstall build_dir prefix =
  cmd "cmake" ["--install", build_dir, "--prefix", prefix] Nothing Nothing

isGlobMatch :: FilePath -> [FilePattern] -> Bool
isGlobMatch f = any (?== f)
