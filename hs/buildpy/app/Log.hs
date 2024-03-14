module Log where

import Data.Time

timeFunction :: String -> IO () -> IO ()
timeFunction desc function = do
   startTime <- getCurrentTime
   function
   endTime <- getCurrentTime
   let diff = diffUTCTime endTime startTime
   putStrLn $ desc ++ " execution Time: " ++ show diff


data LogType = DEBUG | INFO | WARN | ERROR

cyan :: String -> String
cyan s = "\ESC[36m" ++ s ++ "\ESC[0m"

magenta :: String -> String
magenta s = "\ESC[35m" ++ s ++ "\ESC[0m"

green :: String -> String
green s = "\ESC[92m" ++ s ++ "\ESC[0m"

yellow :: String -> String
yellow s = "\ESC[93m" ++ s ++ "\ESC[0m"


logmsg :: LogType -> String -> IO ()
logmsg t msg = do
   timestamp <- formatTime defaultTimeLocale "%H:%M:%S" <$> getCurrentTime
   case (t, msg) of
      (DEBUG, _) -> putStrLn $ timestamp ++ " " ++ cyan "DEBUG " ++ msg
      (INFO, _)  -> putStrLn $ timestamp ++ " " ++ green "INFO " ++ msg
      (WARN, _)  -> putStrLn $ timestamp ++ " " ++ yellow "WARN " ++ msg
      (ERROR, _) -> putStrLn $ timestamp ++ " " ++ magenta "ERROR " ++ msg

info :: String -> IO ()
info = logmsg INFO

debug :: String -> IO ()
debug = logmsg DEBUG

warn :: String -> IO ()
warn = logmsg WARN

error :: String -> IO ()
error = logmsg ERROR
