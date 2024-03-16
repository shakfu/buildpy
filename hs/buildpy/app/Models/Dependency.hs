module Models.Dependency where

import Models.Project
import Utils (replace)
import System.FilePath (joinPath)

data DependencyConfig = DependencyConfig
    { depName :: String
    , depVersion :: String
    , depRepoUrl :: String
    , depRepoBranch :: String
    , depDownloadUrl :: String
    , depOptions :: [String]
    , depLibs :: [String]
    } deriving (Show)

depPrefix :: Project -> DependencyConfig -> FilePath
depPrefix p c = joinPath [projectInstall p, depName c]

depSrcDir :: Project -> DependencyConfig -> FilePath
depSrcDir p c = joinPath [projectSrc p, depName c]

depBuildDir :: Project -> DependencyConfig -> FilePath
depBuildDir p c = joinPath [depSrcDir p c, "build"]



sslConfig :: String -> DependencyConfig
sslConfig v =
    DependencyConfig
        { depName = "openssl"
        , depVersion = v ++ "w"
        , depRepoUrl = "https://github.com/openssl/openssl.git"
        , depRepoBranch = "OpenSSL_" ++ replace '.' '_' v ++ "w"
        , depDownloadUrl =
              "https://www.openssl.org/source/old/"
                  ++ v
                  ++ "/openssl-"
                  ++ v
                  ++ "w.tar.gz"
        , depOptions = []
        , depLibs = ["libssl.a", "libcrypto.a"]
        }

xzConfig :: String -> DependencyConfig
xzConfig v =
    DependencyConfig
        { depName = "xz"
        , depVersion = v
        , depRepoUrl = "https://github.com/tukaani-project/xz.git"
        , depRepoBranch = "v" ++ v
        , depDownloadUrl =
              "https://github.com/tukaani-project/xz/releases/download/v"
                  ++ v
                  ++ "/xz-"
                  ++ v
                  ++ ".tar.gz"
        , depOptions = []
        , depLibs = ["liblzma.a"]
        }

bz2Config :: String -> DependencyConfig
bz2Config v =
    DependencyConfig
        { depName = "bzip2"
        , depVersion = v
        , depRepoUrl = "https://github.com/libarchive/bzip2.git"
        , depRepoBranch = "bzip2-" ++ v
        , depDownloadUrl =
              "https://sourceware.org/pub/bzip2/bzip2-" ++ v ++ ".tar.gz"
        , depOptions = []
        , depLibs = ["libbz2.a"]
        }
