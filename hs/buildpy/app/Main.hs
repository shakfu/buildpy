module Main where

import System.Process ( shell, CreateProcess, createProcess, proc )

import Config (configName, defaultConfig)

ls :: CreateProcess
ls = shell "ls"



main :: IO ()
main = do 
    _ <- createProcess (proc "echo" ["hello"])
    putStrLn (configName defaultConfig)
