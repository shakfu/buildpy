module Dependency.Config where


import System.FilePath (joinPath)
import Text.Show.Functions ()


import Dependency.Model ( Dependency(..) )
import Log (info)
import Process (cmd)
import Project ( Project )
import Shell ( cmakeConfig, cmakeBuild, cmakeInstall )
import Types
    ( Buildable(srcDir, buildDir, download, prefix), Version )
import Utils (replace)

-- ----------------------------------------------------------------------------
-- openssl
sslConfig :: Version -> Project -> Dependency
sslConfig version proj =
    Dependency
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
        , depBuildFunc = buildSsl
        }

buildSsl :: Dependency -> IO ()
buildSsl d = do
    info $ "building " ++ depName d
    download d
    let args =
            ["./config", "no-shared", "no-tests"] ++ ["--prefix=" ++ prefix d]
    let srcdir = Just $ srcDir d
    Process.cmd "bash" args srcdir Nothing
    Process.cmd "make" ["install_sw"] srcdir Nothing

-- ----------------------------------------------------------------------------
-- xz (lzma)
xzConfig :: Version -> Project -> Dependency
xzConfig version proj =
    Dependency
        { depName = "xz"
        , depVersion = version
        , depRepoUrl = "https://github.com/python/cpython-source-deps.git"
        , depRepoBranch = "xz"
        , depDownloadUrl =
              "https://github.com/tukaani-project/xz/releases/download/v"
                  ++ version
                  ++ "/xz-"
                  ++ version
                  ++ ".tar.gz" -- not used
        , depOptions = []
        , depLibs = ["liblzma.a"]
        , depProject = proj
        , depBuildFunc = buildXz
        }

buildXz :: Dependency -> IO ()
buildXz d = do
    info $ "building " ++ depName d
    download d
    let args =
            [ "configure"
            , "--disable-dependency-tracking"
            , "--disable-xzdec"
            , "--disable-lzmadec"
            , "--disable-nls"
            , "--enable-small"
            , "--disable-shared"
            ]
                ++ ["--prefix=" ++ prefix d]
    let srcdir = Just $ srcDir d
    Process.cmd
        "chmod"
        ["+x", joinPath ["build-aux", "install-sh"]]
        srcdir
        Nothing
    Process.cmd "/bin/sh" args srcdir Nothing
    Process.cmd "make" ["install"] srcdir Nothing

-- only to be used with SAFE newer versions of xz
buildXz' :: Dependency -> IO ()
buildXz' d = do
    info $ "building " ++ depName d
    download d
    Shell.cmakeConfig
        (srcDir d)
        (buildDir d)
        [ "-DBUILD_SHARED_LIBS=OFF"
        , "-DENABLE_NLS=OFF"
        , "-DENABLE_SMALL=ON"
        , "-DCMAKE_BUILD_TYPE=MinSizeRel"
        ]
        (Just [("CFLAGS", "-fPIC")])
    Shell.cmakeBuild (buildDir d) False
    Shell.cmakeInstall (buildDir d) (prefix d)

-- ----------------------------------------------------------------------------
-- bzip2
bz2Config :: Version -> Project -> Dependency
bz2Config version proj =
    Dependency
        { depName = "bzip2"
        , depVersion = version
        , depRepoUrl = "https://github.com/libarchive/bzip2.git"
        , depRepoBranch = "bzip2-" ++ version
        , depDownloadUrl =
              "https://sourceware.org/pub/bzip2/bzip2-" ++ version ++ ".tar.gz"
        , depOptions = []
        , depLibs = ["libbz2.a"]
        , depProject = proj
        , depBuildFunc = buildBz2
        }

buildBz2 :: Dependency -> IO ()
buildBz2 d = do
    info $ "building " ++ depName d
    download d
    let args = ["install", "CFLAGS=-fPIC"] ++ ["PREFIX=" ++ prefix d]
    let srcdir = Just $ srcDir d
    Process.cmd "make" args srcdir Nothing
