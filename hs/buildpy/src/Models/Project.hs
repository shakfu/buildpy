module Models.Project where

import System.Directory (getCurrentDirectory)
import System.FilePath (joinPath)

import Shell (makedir, remove)

data Project = Project
    { projectCwd :: FilePath
    , projectBuild :: FilePath
    , projectDownloads :: FilePath
    , projectSrc :: FilePath
    , projectInstall :: FilePath
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

resetProject :: Project -> IO ()
resetProject p = do
    mapM_ remove [projectSrc p, pythonPrefix]
  where
    pythonPrefix = joinPath [projectInstall p, "python"]
