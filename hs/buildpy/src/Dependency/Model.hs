{-# LANGUAGE InstanceSigs #-}

module Dependency.Model where

import System.FilePath (joinPath)
import Text.Show.Functions ()

import Project ( Project(projectSrc, projectInstall) )
import Shell ( gitClone )
import Types ( Buildable(..), Url, Name, Version )

data Dependency = Dependency
    { depName :: Name
    , depVersion :: Version
    , depRepoUrl :: Url
    , depRepoBranch :: String
    , depDownloadUrl :: Url
    , depOptions :: [String]
    , depLibs :: [String]
    , depProject :: Project
    , depBuildFunc :: Dependency -> IO ()
    } deriving (Show)

instance Buildable Dependency where
    prefix :: Dependency -> FilePath
    prefix d = joinPath [projectInstall $ depProject d, depName d]
    srcDir :: Dependency -> FilePath
    srcDir d = joinPath [projectSrc $ depProject d, depName d]
    buildDir :: Dependency -> FilePath
    buildDir d = joinPath [srcDir d, "build"]
    download :: Dependency -> IO ()
    download d = do
        Shell.gitClone url branch dir False
      where
        url = depRepoUrl d
        branch = depRepoBranch d
        dir = srcDir d
    build :: Dependency -> IO ()
    build d = depBuildFunc d d
