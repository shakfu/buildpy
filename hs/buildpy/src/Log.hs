module Log where

import Data.Time (defaultTimeLocale, diffUTCTime, formatTime, getCurrentTime)

data LogLevel
    = DEBUG
    | INFO
    | WARN
    | ERROR

cyan :: String -> String
cyan s = "\ESC[36m" ++ s ++ "\ESC[0m"

magenta :: String -> String
magenta s = "\ESC[35m" ++ s ++ "\ESC[0m"

green :: String -> String
green s = "\ESC[92m" ++ s ++ "\ESC[0m"

yellow :: String -> String
yellow s = "\ESC[93m" ++ s ++ "\ESC[0m"

withLogLevel :: LogLevel -> String -> IO ()
withLogLevel level msg = do
    timestamp <- formatTime defaultTimeLocale "%H:%M:%S" <$> getCurrentTime
    case level of
        DEBUG -> putStrLn $ timestamp ++ " " ++ cyan "DEBUG " ++ msg
        INFO -> putStrLn $ timestamp ++ " " ++ green "INFO " ++ msg
        WARN -> putStrLn $ timestamp ++ " " ++ yellow "WARN " ++ msg
        ERROR -> putStrLn $ timestamp ++ " " ++ magenta "ERROR " ++ msg

logInfo :: String -> IO ()
logInfo = withLogLevel INFO

logDebug :: String -> IO ()
logDebug = withLogLevel DEBUG

logWarn :: String -> IO ()
logWarn = withLogLevel WARN

logError :: String -> IO ()
logError = withLogLevel ERROR

timeFunction :: String -> IO () -> IO ()
timeFunction desc function = do
    startTime <- getCurrentTime
    function
    endTime <- getCurrentTime
    let diff = diffUTCTime endTime startTime
    logInfo $ desc ++ " execution Time: " ++ show diff
