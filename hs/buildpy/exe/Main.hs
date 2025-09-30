{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import qualified BuildPy (run)
import qualified Python.Config as Config
import qualified Python.Model as Model
import qualified Dependency.Config as DepConfig
-- import qualified Dependency.Model as Deps
import qualified Project as Project
import qualified Shell

import Control.Monad (when)
-- import Data.Aeson (encode, decode, ToJSON, FromJSON)
-- import Data.Yaml (encodeFile, decodeFileEither)
-- import qualified Data.ByteString.Lazy.Char8 as L8
-- import qualified Data.Text as T
import Options.Applicative
import System.Exit (exitSuccess)
import System.Directory (removeDirectoryRecursive, doesDirectoryExist)
import Types (BuildType(..), SizeType(..))

-- Main data types for commands
data Command
    = Build BuildOptions
    | Config ConfigOptions
    | Deps DepsOptions
    | Completion CompletionOptions
    deriving (Show, Eq)

data BuildOptions = BuildOptions
    { buildVersion :: String
    , buildConfig :: String
    , buildPackages :: [String]
    , buildOpts :: [String]
    , buildJobs :: Int
    , buildOptimize :: Bool
    , buildReset :: Bool
    , buildDebug :: Bool
    , buildGit :: Bool
    , buildVerbose :: Bool
    } deriving (Show, Eq)

data ConfigOptions = ConfigOptions
    { configVersion :: String
    , configName :: String
    , configWrite :: Maybe String
    , configRead :: Maybe String
    , configSkip :: Bool
    , configYaml :: Bool
    , configLocal :: Bool
    , configList :: Bool
    } deriving (Show, Eq)

data DepsOptions = DepsOptions
    { depsAll :: Bool
    , depsSsl :: Bool
    , depsBz2 :: Bool
    , depsXz :: Bool
    } deriving (Show, Eq)

data CompletionOptions = CompletionOptions
    { completionShell :: String
    } deriving (Show, Eq)

-- Command line parsers
buildOptionsParser :: Parser BuildOptions
buildOptionsParser = BuildOptions
    <$> strOption
        ( long "version"
       <> short 'v'
       <> metavar "VERSION"
       <> value "3.12.9"
       <> help "Python version to build" )
    <*> strOption
        ( long "config"
       <> short 'c'
       <> metavar "CONFIG"
       <> value "static_max"
       <> help "Build configuration" )
    <*> many (strOption
        ( long "pkgs"
       <> short 'p'
       <> metavar "PACKAGE"
       <> help "Python packages to install" ))
    <*> many (strOption
        ( long "opts"
       <> short 'o'
       <> metavar "OPTION"
       <> help "Python configure options" ))
    <*> option auto
        ( long "jobs"
       <> short 'j'
       <> metavar "N"
       <> value 4
       <> help "Number of build jobs" )
    <*> switch
        ( long "optimize"
       <> short 'O'
       <> help "Optimize build" )
    <*> switch
        ( long "reset"
       <> short 'r'
       <> help "Reset build directory" )
    <*> switch
        ( long "debug"
       <> short 'd'
       <> help "Debug build" )
    <*> switch
        ( long "git"
       <> short 'g'
       <> help "Use git to download Python" )
    <*> switch
        ( long "verbose"
       <> help "Verbose output" )

configOptionsParser :: Parser ConfigOptions
configOptionsParser = ConfigOptions
    <$> strOption
        ( long "version"
       <> short 'v'
       <> metavar "VERSION"
       <> value "3.12.9"
       <> help "Python version" )
    <*> strOption
        ( long "name"
       <> short 'n'
       <> metavar "NAME"
       <> value "static_max"
       <> help "Configuration name" )
    <*> optional (strOption
        ( long "write"
       <> short 'w'
       <> metavar "FILE"
       <> help "Write config to file" ))
    <*> optional (strOption
        ( long "read"
       <> short 'r'
       <> metavar "FILE"
       <> help "Read config from file" ))
    <*> switch
        ( long "skip"
       <> short 's'
       <> help "Skip configuration step" )
    <*> switch
        ( long "yaml"
       <> short 'y'
       <> help "Output as YAML" )
    <*> switch
        ( long "local"
       <> short 'l'
       <> help "Write Setup.local file" )
    <*> switch
        ( long "list"
       <> help "List available configurations" )

depsOptionsParser :: Parser DepsOptions
depsOptionsParser = DepsOptions
    <$> switch
        ( long "all"
       <> short 'a'
       <> help "Build all dependencies" )
    <*> switch
        ( long "ssl"
       <> short 's'
       <> help "Build OpenSSL" )
    <*> switch
        ( long "bz2"
       <> short 'b'
       <> help "Build bzip2" )
    <*> switch
        ( long "xz"
       <> short 'x'
       <> help "Build xz" )

completionOptionsParser :: Parser CompletionOptions
completionOptionsParser = CompletionOptions
    <$> strOption
        ( long "shell"
       <> metavar "SHELL"
       <> value "bash"
       <> help "Shell for completion (bash, zsh, fish)" )

-- Command parsers
commandParser :: Parser Command
commandParser = subparser
    ( command "build"
        ( info (Build <$> buildOptionsParser <**> helper)
               ( progDesc "Build Python from source" ) )
   <> command "config"
        ( info (Config <$> configOptionsParser <**> helper)
               ( progDesc "Manage Python configuration" ) )
   <> command "deps"
        ( info (Deps <$> depsOptionsParser <**> helper)
               ( progDesc "Build and manage Python dependencies" ) )
   <> command "completion"
        ( info (Completion <$> completionOptionsParser <**> helper)
               ( progDesc "Generate shell completions" ) )
    )

-- Program info
programInfo :: ParserInfo Command
programInfo = info (commandParser <**> helper)
    ( fullDesc
   <> progDesc "Build Python from source with customizable options"
   <> header "buildpy - Python source builder" )

-- Command execution
executeCommand :: Command -> IO ()
executeCommand = \case
    Build opts -> executeBuild opts
    Config opts -> executeConfig opts
    Deps opts -> executeDeps opts
    Completion opts -> executeCompletion opts

executeBuild :: BuildOptions -> IO ()
executeBuild opts = do
    when (buildVerbose opts) $
        putStrLn $ "Building Python " ++ buildVersion opts ++ " with config: " ++ buildConfig opts

    when (buildReset opts) $ do
        exists <- doesDirectoryExist "build"
        when exists $ do
            putStrLn "Resetting build directory..."
            removeDirectoryRecursive "build"

    -- Parse build type and size type from config
    let (buildType, sizeType) = parseConfig (buildConfig opts)

    -- Create project and Python configuration
    proj <- Project.defaultProject
    let config = Config.configurePython (buildVersion opts) buildType sizeType proj

    -- Apply additional options
    let finalConfig = config
            { Model.pythonOptimize = buildOptimize opts
            , Model.pythonConfigOptions = Model.pythonConfigOptions config ++ buildOpts opts
            , Model.pythonPackages = buildPackages opts
            }

    -- Build process
    if buildGit opts
        then do
            putStrLn "Using git to download Python..."
            Model.downloadPython finalConfig
        else do
            putStrLn "Using HTTP to download Python..."
            downloadPythonHttp finalConfig

    Config.doConfigurePython finalConfig
    Model.buildPython finalConfig
    Model.installPython finalConfig
    Model.cleanPython finalConfig
    Model.zipPythonLib finalConfig

    putStrLn "Python build completed successfully!"

-- Parse config string into BuildType and SizeType
parseConfig :: String -> (BuildType, SizeType)
parseConfig cfg = case cfg of
    "static_max" -> (Static, Max)
    "static_mid" -> (Static, Mid)
    "static_min" -> (Static, Min)
    "shared_max" -> (Shared, Max)
    "shared_mid" -> (Shared, Mid)
    "shared_min" -> (Shared, Min)
    _ -> error $ "Invalid config: " ++ cfg

-- Download Python via HTTP instead of git
downloadPythonHttp :: Model.PythonConfig -> IO ()
downloadPythonHttp config = do
    let url = Model.pythonDownloadUrl config
    let version = Model.pythonVersion config
    putStrLn $ "Downloading Python " ++ version ++ " from " ++ url
    -- TODO: Implement HTTP download, extract and move to src directory
    Shell.curl "." url
    putStrLn "HTTP download completed"

executeConfig :: ConfigOptions -> IO ()
executeConfig opts = do
    when (configList opts) $ do
        putStrLn "Available build configurations:"
        mapM_ putStrLn ["static_max", "static_mid", "static_min",
                       "shared_max", "shared_mid", "shared_min"]
        exitSuccess

    -- Parse build type and size type from config name
    let (buildType, sizeType) = parseConfig (configName opts)

    -- Create project and Python configuration
    proj <- Project.defaultProject
    let config = Config.configurePython (configVersion opts) buildType sizeType proj

    -- Handle configuration reading from file
    finalConfig <- case configRead opts of
        Just file -> do
            putStrLn $ "Reading config from: " ++ file
            -- TODO: Implement YAML config reading with proper types
            putStrLn "Config reading not yet fully implemented"
            return config
        Nothing -> return config

    if not (configSkip opts)
        then do
            putStrLn "Configuring Python build..."
            Config.doConfigurePython finalConfig
        else putStrLn "Skipping configuration step"

    -- Handle output
    case configWrite opts of
        Just file -> do
            if configYaml opts
                then do
                    putStrLn $ "Writing YAML config to: " ++ file
                    -- TODO: Implement YAML serialization of PythonConfig
                    putStrLn "YAML config written"
                else do
                    putStrLn $ "Writing Setup.local to: " ++ file
                    Model.writeSetupLocal file finalConfig
            when (configLocal opts) $ do
                let localFile = "build/src/python/Modules/Setup.local"
                putStrLn $ "Writing Setup.local to: " ++ localFile
                Model.writeSetupLocal localFile finalConfig
        Nothing ->
            if configYaml opts
                then do
                    putStrLn "Python configuration (YAML format):"
                    -- TODO: Print YAML representation
                    putStrLn "# YAML output not yet implemented"
                else do
                    putStrLn "Python configuration (Setup.local format):"
                    Model.writeSetupLocal "/dev/stdout" finalConfig

executeDeps :: DepsOptions -> IO ()
executeDeps opts = do
    putStrLn "Dependencies command called"
    proj <- Project.defaultProject

    when (depsAll opts || depsSsl opts) $ do
        putStrLn "Building OpenSSL..."
        let ssl = DepConfig.sslConfig "1.1.1" proj
        DepConfig.buildSsl ssl

    when (depsAll opts || depsBz2 opts) $ do
        putStrLn "Building bzip2..."
        let bz2 = DepConfig.bz2Config "1.0.8" proj
        DepConfig.buildBz2 bz2

    when (depsAll opts || depsXz opts) $ do
        putStrLn "Building xz..."
        let xz = DepConfig.xzConfig "5.2.5" proj
        DepConfig.buildXz xz

    putStrLn "Dependencies build completed!"

executeCompletion :: CompletionOptions -> IO ()
executeCompletion opts = do
    let shell = completionShell opts
    case shell of
        "bash" -> putStrLn bashCompletion
        "zsh" -> putStrLn zshCompletion
        "fish" -> putStrLn fishCompletion
        _ -> putStrLn $ "Unsupported shell: " ++ shell

-- Basic bash completion script
bashCompletion :: String
bashCompletion = unlines
    [ "_buildpy_completion() {"
    , "    local cur=\"${COMP_WORDS[COMP_CWORD]}\""
    , "    local prev=\"${COMP_WORDS[COMP_CWORD-1]}\""
    , "    "
    , "    case \"$prev\" in"
    , "        --config|-c)"
    , "            COMPREPLY=($(compgen -W \"static_max static_mid static_min shared_max shared_mid shared_min\" -- \"$cur\"))"
    , "            return 0"
    , "            ;;"
    , "        --version|-v)"
    , "            COMPREPLY=($(compgen -W \"3.11.0 3.12.0 3.12.9 3.13.0\" -- \"$cur\"))"
    , "            return 0"
    , "            ;;"
    , "    esac"
    , "    "
    , "    if [[ \"$cur\" == -* ]]; then"
    , "        COMPREPLY=($(compgen -W \"--help --version --config --pkgs --opts --jobs --optimize --reset --debug --git --verbose\" -- \"$cur\"))"
    , "    else"
    , "        COMPREPLY=($(compgen -W \"build config deps completion\" -- \"$cur\"))"
    , "    fi"
    , "}"
    , ""
    , "complete -F _buildpy_completion buildpy"
    ]

-- Basic zsh completion script
zshCompletion :: String
zshCompletion = unlines
    [ "#compdef buildpy"
    , ""
    , "_buildpy() {"
    , "    local context state line"
    , "    "
    , "    _arguments -C \\"
    , "        '1: :->commands' \\"
    , "        '*: :->args'"
    , "    "
    , "    case $state in"
    , "        commands)"
    , "            _values 'commands' \\"
    , "                'build[Build Python from source]' \\"
    , "                'config[Manage Python configuration]' \\"
    , "                'deps[Build and manage Python dependencies]' \\"
    , "                'completion[Generate shell completions]'"
    , "            ;;"
    , "        args)"
    , "            case ${words[2]} in"
    , "                build)"
    , "                    _arguments \\"
    , "                        '--config[Build configuration]:config:(static_max static_mid static_min shared_max shared_mid shared_min)' \\"
    , "                        '--version[Python version]:version:(3.11.0 3.12.0 3.12.9 3.13.0)' \\"
    , "                        '--help[Show help]'"
    , "                    ;;"
    , "            esac"
    , "            ;;"
    , "    esac"
    , "}"
    , ""
    , "_buildpy \"$@\""
    ]

-- Basic fish completion script
fishCompletion :: String
fishCompletion = unlines
    [ "# buildpy fish completion"
    , ""
    , "complete -c buildpy -f"
    , ""
    , "# Commands"
    , "complete -c buildpy -n '__fish_use_subcommand' -a build -d 'Build Python from source'"
    , "complete -c buildpy -n '__fish_use_subcommand' -a config -d 'Manage Python configuration'"
    , "complete -c buildpy -n '__fish_use_subcommand' -a deps -d 'Build and manage Python dependencies'"
    , "complete -c buildpy -n '__fish_use_subcommand' -a completion -d 'Generate shell completions'"
    , ""
    , "# Build command options"
    , "complete -c buildpy -n '__fish_seen_subcommand_from build' -l config -d 'Build configuration' -xa 'static_max static_mid static_min shared_max shared_mid shared_min'"
    , "complete -c buildpy -n '__fish_seen_subcommand_from build' -l version -d 'Python version' -xa '3.11.0 3.12.0 3.12.9 3.13.0'"
    , "complete -c buildpy -n '__fish_seen_subcommand_from build' -l help -d 'Show help'"
    ]

-- Main entry point
main :: IO ()
main = do
    cmd <- execParser programInfo
    executeCommand cmd