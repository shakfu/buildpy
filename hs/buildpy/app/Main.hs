module Main where

import Data.Maybe (fromMaybe)
import System.Console.GetOpt

-- import Prelude hiding (log)
import System.Environment (getArgs, getProgName)

-- import Config (configName, defaultConfig)
-- import Log (info, timeFunction)
import Control.Monad
-- import Process (cmd, run)
-- import Shell (isGlobMatch)
someFunction :: IO ()
someFunction = do
    putStrLn "Function completed"
   -- gitClone "https://github.com/python/cpython.git" "v3.12.2" "python" False 

-- demo :: IO ()    
-- demo = do
-- main :: IO ()
-- main = do
--     let _ = cmd "ls" [] Nothing Nothing
--     info "running python"
--    -- run "python3" ["-c", "import sys; print(sys.version)"]
--     run "python3" ["--version"]
--     putStrLn (configName defaultConfig)
--     timeFunction "someFunction" someFunction
--    -- demo


header = "Usage: newsagent [options]"

-- option types
data Flag = Verbose | NoHtml | Help | List | Parallel | Name String
            deriving (Show, Eq)

defaultMission = Name . fromMaybe "tech"

-- commandline options
options :: [OptDescr Flag]
options = 
    [ Option "h" ["help"]     (NoArg Help)     "display help"
    , Option "v" ["verbose"]  (NoArg Verbose)  "show verbose output"
    , Option "l" ["list"]     (NoArg List)     "list available missions"
    , Option "n" ["nohtml"]   (NoArg NoHtml)   "skip html output"
    , Option "p" ["parallel"] (NoArg Parallel) "runs agents in parallel"
    , Option "m" ["mission"]  (OptArg defaultMission "MISSION") "set mission"   
    ]

-- primary command line processing function
processArgs :: [Flag] -> IO ()
processArgs flags = do
    when (Help    `elem` flags) $ putStrLn $ usageInfo header options
    when (Verbose `elem` flags) $ dump flags
    -- when (List    `elem` flags) $ mapM_ putStrLn [missionCodeName m | m <- missions]
    -- startMissionFromFlags flags
    where
        dump fs = putStrLn $ "options: " ++ show fs
        -- startMissionFromFlags fs = case fs of
        --     [Name s] -> do
        --         putStrLn $ "starting mission: " ++ s
        --         startMissionByName s
        --     [Parallel] -> do 
        --         putStrLn "running agents in parallel..."
        --         start_p missions
        --     [NoHtml] -> do
        --         putStrLn "skipping html generation..."
        --         start missions
        --     [_]      -> putStr ""




----------------------------------------------------------------------
-- main entrypoint
--
-- note:
----------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case getOpt RequireOrder options args of
        ([],    [],      [])   -> putStrLn "EMPTY" --start missions 
        (flags, [],      [])   -> processArgs flags
        (_,     nonOpts, [])   -> error $ "unrecognized arguments: " ++ unwords nonOpts
        (_,     _,       msgs) -> error $ concat msgs ++ usageInfo header options
