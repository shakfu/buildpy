{-# LANGUAGE LambdaCase #-}

module Main where

import qualified BuildPy (run)

import Control.Monad (when)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)


header :: String -> String
header name = "Usage: " ++ name ++ " [options]\n\nA Python builder - builds Python from source with customizable options"

-- Constants
defaultPyVersion :: String
defaultPyVersion = "3.12.9"

defaultConfig :: String
defaultConfig = "static_max"

defaultJobs :: Int
defaultJobs = 4

-- Option types
data Flag
    = CfgOpts [String]       -- -a, --cfg-opts
    | Config String          -- -c, --config
    | Debug                  -- -d, --debug
    | Help                   -- -h, --help
    | Jobs Int              -- -j, --jobs
    | Optimize              -- -o, --optimize
    | Pkgs [String]         -- -p, --pkgs
    | Reset                 -- -r, --reset
    | JsonFile String       -- -s, --json
    | Version String        -- -v, --version
    | Verbose               -- --verbose
    | Write                 -- -w, --write
    | List                  -- -l, --list (kept for compatibility)
    deriving (Show, Eq)

-- Helper functions for parsing multi-value options
readCfgOpts :: String -> Flag
readCfgOpts s = CfgOpts (words s)

readPkgs :: String -> Flag
readPkgs s = Pkgs (words s)

readJobs :: String -> Flag
readJobs s = case reads s of
    [(n, "")] | n > 0 -> Jobs n
    _ -> error $ "Invalid number of jobs: " ++ s ++ " (must be positive integer)"

validateConfig :: String -> String
validateConfig config
    | config `elem` validConfigs = config
    | otherwise = error $ "Invalid config: " ++ config ++ ". Valid configs: " ++ show validConfigs
  where
    validConfigs = ["static_max", "static_mid", "static_min", "shared_max", "shared_mid", "shared_min"]

validateVersion :: String -> String
validateVersion version
    | length parts == 3 && all isValidVersionPart parts = version
    | otherwise = error $ "Invalid Python version format: " ++ version ++ " (expected X.Y.Z format)"
  where
    parts = splitOn '.' version
    isValidVersionPart s = not (null s) && all isDigit s
    isDigit c = c >= '0' && c <= '9'

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn delimiter str =
    let (before, remainder) = span (/= delimiter) str
    in before : case remainder of
                  [] -> []
                  (_:after) -> splitOn delimiter after

-- Command line options
options :: [OptDescr Flag]
options =
    [ Option "a" ["cfg-opts"] (ReqArg readCfgOpts "CFG") "add config options"
    , Option "c" ["config"] (ReqArg (Config . validateConfig) "NAME") "build configuration (default: static_max)"
    , Option "d" ["debug"] (NoArg Debug) "build debug python"
    , Option "h" ["help"] (NoArg Help) "display help"
    , Option "j" ["jobs"] (ReqArg readJobs "N") "# of build jobs (default: 4)"
    , Option "l" ["list"] (NoArg List) "list available build configs"
    , Option "o" ["optimize"] (NoArg Optimize) "optimize build"
    , Option "p" ["pkgs"] (ReqArg readPkgs "PKG") "install packages"
    , Option "r" ["reset"] (NoArg Reset) "reset build"
    , Option "s" ["json"] (ReqArg JsonFile "FILE") "serialize config to json file"
    , Option "v" ["version"] (ReqArg (Version . validateVersion) "VER") "python version (default: 3.12.9)"
    , Option "" ["verbose"] (NoArg Verbose) "show verbose output"
    , Option "w" ["write"] (NoArg Write) "write configuration"
    ]

-- Extract specific option values from flags
getConfig :: [Flag] -> String
getConfig flags = case [s | Config s <- flags] of
    [] -> defaultConfig
    (s:_) -> s

getVersion :: [Flag] -> String
getVersion flags = case [s | Version s <- flags] of
    [] -> defaultPyVersion
    (s:_) -> s

getJobs :: [Flag] -> Int
getJobs flags = case [n | Jobs n <- flags] of
    [] -> defaultJobs
    (n:_) -> n

getCfgOpts :: [Flag] -> [String]
getCfgOpts flags = concatMap (\case CfgOpts opts -> opts; _ -> []) flags

getPkgs :: [Flag] -> [String]
getPkgs flags = concatMap (\case Pkgs pkgs -> pkgs; _ -> []) flags

getJsonFile :: [Flag] -> Maybe String
getJsonFile flags = case [f | JsonFile f <- flags] of
    [] -> Nothing
    (f:_) -> Just f

-- Check if a flag is present
hasFlag :: Flag -> [Flag] -> Bool
hasFlag flag flags = any (== flag) (map (\f -> case f of
    CfgOpts _ -> CfgOpts []
    Config _ -> Config ""
    Version _ -> Version ""
    Jobs _ -> Jobs 0
    Pkgs _ -> Pkgs []
    JsonFile _ -> JsonFile ""
    other -> other) flags)

-- Primary command line processing function
processArgs :: [Flag] -> IO ()
processArgs flags = do
    prog <- getProgName

    -- Handle help first
    when (Help `elem` flags) $ do
        putStrLn $ usageInfo (header prog) options
        exitSuccess

    when (Verbose `elem` flags) $
        putStrLn $ "options: " ++ show flags

    when (List `elem` flags) $ do
        putStrLn "Available build configs:"
        mapM_ putStrLn ["static_max", "static_mid", "static_min",
                       "shared_max", "shared_mid", "shared_min"]
        exitSuccess

    -- TODO: Handle write and json options
    when (Write `elem` flags) $
        putStrLn "Write configuration not yet implemented"

    when (Reset `elem` flags) $
        putStrLn "Reset build not yet implemented"

    -- Extract parameters for BuildPy.run
    let config = getConfig flags
        version = getVersion flags

    putStrLn $ "Building Python " ++ version ++ " with config: " ++ config
    BuildPy.run version config

----------------------------------------------------------------------
-- Main entrypoint
----------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    case getOpt RequireOrder options args of
        ([], [], []) -> BuildPy.run defaultPyVersion defaultConfig
        (flags, [], []) -> processArgs flags
        (_, nonOpts, []) ->
            error $ "unrecognized arguments: " ++ unwords nonOpts
        (_, _, msgs) -> do
            putStrLn $ concat msgs
            putStrLn $ usageInfo (header prog) options
