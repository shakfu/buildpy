module Models.Dependency where

import Log (info)
import Models.Project
import Process
import Shell
import System.FilePath (joinPath)
import Text.Show.Functions ()
import Utils (replace)

data DependencyConfig = DependencyConfig
  { depName :: String
  , depVersion :: String
  , depRepoUrl :: String
  , depRepoBranch :: String
  , depDownloadUrl :: String
  , depOptions :: [String]
  , depLibs :: [String]
  , depProject :: Project
  , depBuildFunc :: DependencyConfig -> IO ()
  } deriving (Show)

depPrefix :: DependencyConfig -> FilePath
depPrefix c = joinPath [projectInstall $ depProject c, depName c]

depSrcDir :: DependencyConfig -> FilePath
depSrcDir c = joinPath [projectSrc $ depProject c, depName c]

depBuildDir :: DependencyConfig -> FilePath
depBuildDir c = joinPath [depSrcDir c, "build"]

downloadDep :: DependencyConfig -> IO ()
downloadDep c = do
  Shell.gitClone url branch dir False
  where
    url = depRepoUrl c
    branch = depRepoBranch c
    dir = depSrcDir c

processSsl :: DependencyConfig -> IO ()
processSsl c = do
  info $ "building " ++ depName c
  downloadDep c
  let args =
        ["./config", "no-shared", "no-tests"] ++ ["--prefix=" ++ depPrefix c]
  let srcdir = Just $ depSrcDir c
  cmd "bash" args srcdir Nothing
  cmd "make" ["install_sw"] srcdir Nothing

sslConfig :: String -> Project -> DependencyConfig
sslConfig version proj =
  DependencyConfig
    { depName = "openssl"
    , depVersion = version ++ "w"
    , depRepoUrl = "https://github.com/openssl/openssl.git"
    , depRepoBranch = "OpenSSL_" ++ replace '.' '_' version ++ "w"
    , depDownloadUrl =
        "https://www.openssl.org/source/old/"
          ++ version
          ++ "/openssl-"
          ++ version
          ++ "w.tar.gz"
    , depOptions = []
    , depLibs = ["libssl.a", "libcrypto.a"]
    , depProject = proj
    , depBuildFunc = processSsl
    }

processXz :: DependencyConfig -> IO ()
processXz c = do
  info $ "building " ++ depName c
  downloadDep c
  let args =
        ["./configure", "--disable-shared", "--enable-static"]
          ++ ["--prefix=" ++ depPrefix c]
  let srcdir = Just $ depSrcDir c
  cmd "bash" args srcdir Nothing
  cmd "make" ["install"] srcdir Nothing

xzConfig :: String -> Project -> DependencyConfig
xzConfig version proj =
  DependencyConfig
    { depName = "xz"
    , depVersion = version
    , depRepoUrl = "https://github.com/tukaani-project/xz.git"
    , depRepoBranch = "v" ++ version
    , depDownloadUrl =
        "https://github.com/tukaani-project/xz/releases/download/v"
          ++ version
          ++ "/xz-"
          ++ version
          ++ ".tar.gz"
    , depOptions = []
    , depLibs = ["liblzma.a"]
    , depProject = proj
    , depBuildFunc = processXz
    }

processBz2 :: DependencyConfig -> IO ()
processBz2 c = do
  info $ "building " ++ depName c
  downloadDep c
  let args = ["install", "CFLAGS=-fPIC"] ++ ["PREFIX=" ++ depPrefix c]
  let srcdir = Just $ depSrcDir c
  cmd "make" args srcdir Nothing

bz2Config :: String -> Project -> DependencyConfig
bz2Config version proj =
  DependencyConfig
    { depName = "bzip2"
    , depVersion = version
    , depRepoUrl = "https://github.com/libarchive/bzip2.git"
    , depRepoBranch = "bzip2-" ++ version
    , depDownloadUrl =
        "https://sourceware.org/pub/bzip2/bzip2-" ++ version ++ ".tar.gz"
    , depOptions = []
    , depLibs = ["libbz2.a"]
    , depProject = proj
    , depBuildFunc = processBz2
    }
