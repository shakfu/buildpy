module Models.Project where

import System.Directory (getCurrentDirectory)
import System.FilePath (joinPath)

import Shell (makedir)

data Project = Project
  { projectCwd :: String
  , projectBuild :: String
  , projectDownloads :: String
  , projectSrc :: String
  , projectInstall :: String
  } deriving (Show)

newProject :: String -> Project
newProject cwd =
  Project
    { projectCwd = cwd
    , projectBuild = joinPath [cwd, "build"]
    , projectDownloads = joinPath [cwd, "build", "downloads"]
    , projectSrc = joinPath [cwd, "build", "src"]
    , projectInstall = joinPath [cwd, "build", "install"]
    }

defaultProject :: IO Project
defaultProject = do
  newProject <$> getCurrentDirectory

setupProject :: Project -> IO ()
setupProject p = do
  mapM_
    makedir
    [projectBuild p, projectDownloads p, projectSrc p, projectInstall p]
