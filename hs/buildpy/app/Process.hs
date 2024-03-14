module Process where

import System.Process ( shell, CreateProcess, createProcess, proc )


ls :: CreateProcess
ls = shell "ls"


run :: String -> [String] -> IO ()
run exec args = do
    _ <- createProcess (proc exec args)
    putStrLn (exec ++ " " ++ unwords args ++ " DONE")
