module Main where

import qualified BuildPy (run)

-- import Prelude hiding (log)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import System.Console.GetOpt

-- import System.Console.GetOpt
--   ( ArgDescr(NoArg, OptArg)
--   , ArgOrder(RequireOrder)
--   , OptDescr(..)
--   , getOpt
--   , usageInfo
--   )
import System.Environment (getArgs, getProgName)


header :: String -> String
header name = "Usage: " ++ name ++ " [options]"

-- option types
data Flag
    = Verbose
    | Optimize
    | Help
    | List
    | Parallel Int
    | Name String
    | LibDir String
    deriving (Show, Eq)

defaultConfig :: Maybe String -> Flag
defaultConfig = Name . fromMaybe "static_max"

defaultParallel :: Maybe String -> Flag
defaultParallel = Parallel . toInt . fromMaybe "4"
  where
    toInt s = read s :: Int

-- commandline options
options :: [OptDescr Flag]
options =
    [ Option "h" ["help"] (NoArg Help) "display help"
    , Option "v" ["verbose"] (NoArg Verbose) "show verbose output"
    , Option "l" ["list"] (NoArg List) "list available build configs"
    , Option "o" ["optimize"] (NoArg Optimize) "optimize build"
    , Option
          "p"
          ["parallel"]
          (OptArg defaultParallel "JOBS")
          "# of build jobs in parallel"
    , Option "c" ["config"] (OptArg defaultConfig "CONFIG") "set config"
    , Option "L" ["libdir"] (ReqArg LibDir "DIR") "library directory"
    ]

-- primary command line processing function
processArgs :: [Flag] -> IO ()
processArgs flags = do
    prog <- getProgName
    when (Help `elem` flags) $ putStrLn $ usageInfo (header prog) options
    when (Verbose `elem` flags) $ dump flags
  -- when (List    `elem` flags) $ mapM_ putStrLn [pythonConfig i | i <- variants]
    startPythonBuildFromFlags flags
  where
    dump fs = putStrLn $ "options: " ++ show fs
    startPythonBuildFromFlags = print
    -- startPythonBuildFromFlags fs = case fs of
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
    print args
    case getOpt RequireOrder options args of
        ([], [], []) -> BuildPy.run "3.12.2" "static_max"
        (flags, [], []) -> processArgs flags
        (_, nonOpts, []) ->
            error $ "unrecognized arguments: " ++ unwords nonOpts
        (_, _, msgs) -> error $ concat msgs ++ usageInfo (header prog) options
