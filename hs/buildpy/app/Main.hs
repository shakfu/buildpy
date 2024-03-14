module Main where

--import Control.Monad

import Process (run) 
import Config (configName, defaultConfig)
import Log (info, timeFunction)


someFunction :: IO ()
someFunction = do
   putStrLn "Function completed"



main :: IO ()
main = do
   info "running python"
   run "python3" ["-c", "import sys; print(sys.version)"]
   putStrLn (configName defaultConfig)
   timeFunction "someFunction" someFunction
