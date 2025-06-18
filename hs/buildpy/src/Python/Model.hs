{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}

module Python.Model where

import Control.Monad (forM_)
import Data.Map (Map, lookup)
import Data.Maybe (fromJust)
import System.FilePath (joinPath)

import Dependency.Model ( Dependency(depBuildFunc) )
import Log (logInfo)
import Process (cmd)
import Project (Project(projectBuild, projectInstall, projectSrc), setupProject)
import Shell (gitClone, globRemove, makedir, move, remove, zipLib)
import Types
    ( Buildable(..), SizeType, BuildType, Url, Name, Version )
import Utils (lowercase, wordsWhen)

data PythonConfig = PythonConfig
    { pythonName :: Name
    , pythonVersion :: Version
    , pythonBuildType :: BuildType
    , pythonSizeType :: SizeType
    , pythonRepoUrl :: Url
    , pythonRepoBranch :: String
    , pythonDownloadUrl :: Url
    , pythonHeaders :: [String]
    , pythonExts :: Map String [String]
    , pythonCore :: [String]
    , pythonStatic :: [String]
    , pythonShared :: [String]
    , pythonDisabled :: [String]
    , pythonRemovePatterns :: [String]
    , pythonConfigOptions :: [String]
    , pythonPackages :: [String]
    , pythonDependsOn :: [Dependency]
    , pythonOptimize :: Bool
    , pythonProject :: Project
    } deriving (Show)

-- ----------------------------------------------------------------------------
-- properties
instance Buildable PythonConfig where
    prefix :: PythonConfig -> FilePath
    prefix c = joinPath [projectInstall p, lowercase $ pythonName c]
      where
        p = pythonProject c
    srcDir :: PythonConfig -> FilePath
    srcDir c = joinPath [projectSrc p, lowercase $ pythonName c]
      where
        p = pythonProject c
    buildDir :: PythonConfig -> FilePath
    buildDir c = joinPath [srcDir c, "build"]
    download :: PythonConfig -> IO ()
    download c = do
        Shell.gitClone url branch dir False
      where
        url = pythonRepoUrl c
        branch = pythonRepoBranch c
        dir = srcDir c
    build :: PythonConfig -> IO ()
    build c = do
        logInfo ("bui  lding python " ++ show (pythonVersion c))
        cmd "make" [] (Just $ srcDir c) Nothing

-- pythonBuildType :: PythonConfig -> String
-- pythonBuildType c = head $ wordsWhen (== '_') (pythonConfig c)

pythonSetupLocal :: PythonConfig -> FilePath
pythonSetupLocal c = joinPath [srcDir c, "Modules", "Setup.local"]

pythonVer :: PythonConfig -> String
pythonVer c = head v ++ "." ++ v !! 1
  where
    v = wordsWhen (== '.') $ pythonVersion c

nameVer :: PythonConfig -> String
nameVer c = "python" ++ pythonVer c

nameVerNoDot :: PythonConfig -> String
nameVerNoDot c = "python" ++ head v ++ v !! 1
  where
    v = wordsWhen (== '.') $ pythonVersion c

-- ----------------------------------------------------------------------------
-- methods

writeSetupLocal :: FilePath -> PythonConfig -> IO ()
writeSetupLocal file c = do
    let out =
            ["# -*- makefile -*-"]
                ++ pythonHeaders c
                ++ ["\n# core\n"]
                ++ getEntries (pythonCore c)
                ++ ["\n*shared*\n"]
                ++ getEntries (pythonShared c)
                ++ ["\n*static*\n"]
                ++ getEntries (pythonStatic c)
                ++ ["\n*disabled*\n"]
                ++ pythonDisabled c
    writeFile file (unlines out)
  where
    extlookup k = k : fromJust (Data.Map.lookup k (pythonExts c))
    getEntries = Prelude.map (unwords . extlookup)

processPythonDependencies :: PythonConfig -> IO ()
processPythonDependencies c = do
    forM_ (pythonDependsOn c) $ \dep -> do
        depBuildFunc dep dep

downloadPython :: PythonConfig -> IO ()
downloadPython c = do
    Shell.gitClone url branch dir False
  where
    url = pythonRepoUrl c
    branch = pythonRepoBranch c
    dir = srcDir c

setupPython :: PythonConfig -> IO ()
setupPython c = do
    setupProject $ pythonProject c
    processPythonDependencies c

buildPython :: PythonConfig -> IO ()
buildPython c = do
    logInfo ("building python " ++ show (pythonVersion c))
    cmd "make" [] (Just $ srcDir c) Nothing

installPython :: PythonConfig -> IO ()
installPython c = do
    logInfo ("install python " ++ show (pythonVersion c))
    cmd "make" ["install"] (Just $ srcDir c) Nothing

cleanPython :: PythonConfig -> IO ()
cleanPython c = do
    logInfo "cleanPython"
    let path = joinPath [prefix c, "lib", nameVer c]
    globRemove (pythonRemovePatterns c) path

zipPythonLib :: PythonConfig -> IO ()
zipPythonLib c = do
    logInfo "zipPythonLib"
    let src = joinPath [prefix c, "lib", nameVer c]
    let src_libdynload = joinPath [src, "lib-dynload"]
    let src_os_py = joinPath [src, "os.py"]
    let tmp_libdynload =
            joinPath [projectBuild $ pythonProject c, "lib-dynload"]
    let tmp_os_py = joinPath [projectBuild $ pythonProject c, "os.py"]
    let zip_file = joinPath [prefix c, "lib", nameVerNoDot c ++ ".zip"]
    let site_packages = joinPath [src, "site-packages"]
    let pkgconfig = joinPath [prefix c, "lib", "pkgconfig"]
    Shell.move src_libdynload tmp_libdynload
    Shell.move src_os_py tmp_os_py
    Shell.zipLib zip_file src
    mapM_ Shell.remove [src, pkgconfig]
    mapM_ Shell.makedir [src, site_packages]
    Shell.move tmp_libdynload src_libdynload
    Shell.move tmp_os_py src_os_py
