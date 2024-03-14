module Main where

import Prelude hiding (log)

import Config (configName, defaultConfig)
import Log (info, timeFunction)

--import Control.Monad
import Process (cmd, run)

-- import Shell (isGlobMatch)
someFunction :: IO ()
someFunction = do
    putStrLn "Function completed"
   -- gitClone "https://github.com/python/cpython.git" "v3.12.2" "python" False 

-- demo :: IO ()
-- demo = do
main :: IO ()
main = do
    let _ = cmd "ls" [] Nothing Nothing
    info "running python"
   -- run "python3" ["-c", "import sys; print(sys.version)"]
    run "python3" ["--version"]
    putStrLn (configName defaultConfig)
    timeFunction "someFunction" someFunction
   -- demo
