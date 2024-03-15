module Main where

-- import Prelude hiding (log)
import Control.Monad ( when )
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
    ( getOpt,
      usageInfo,
      ArgDescr(OptArg, NoArg),
      ArgOrder(RequireOrder),
      OptDescr(..) )
import System.Environment (getArgs, getProgName)

-- import Config (configName, defaultConfig)
-- import Log (info, timeFunction)

-- import Process (cmd, run)

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

header :: String -> String
header name = "Usage: " ++ name ++ " [options]"

-- option types
data Flag = Verbose | Optimize | Help | List | Parallel | Name String
            deriving (Show, Eq)

defaultConfig :: Maybe String -> Flag
defaultConfig = Name . fromMaybe "static_max"

-- commandline options
options :: [OptDescr Flag]
options = 
    [ Option "h" ["help"]     (NoArg Help)     "display help"
    , Option "v" ["verbose"]  (NoArg Verbose)  "show verbose output"
    , Option "l" ["list"]     (NoArg List)     "list available build configs"
    , Option "o" ["optimize"] (NoArg Optimize) "optimize build"
    , Option "p" ["parallel"] (NoArg Parallel) "build jobs in parallel"
    , Option "c" ["config"]   (OptArg defaultConfig "CONFIG") "set config"   
    ]

-- primary command line processing function
processArgs :: [Flag] -> IO ()
processArgs flags = do
    prog <- getProgName
    when (Help    `elem` flags) $ putStrLn $ usageInfo (header prog) options
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
    prog <- getProgName
    case getOpt RequireOrder options args of
        ([],    [],      [])   -> putStrLn "EMPTY" --start missions 
        (flags, [],      [])   -> processArgs flags
        (_,     nonOpts, [])   -> error $ "unrecognized arguments: " ++ unwords nonOpts
        (_,     _,       msgs) -> error $ concat msgs ++ usageInfo (header prog) options
