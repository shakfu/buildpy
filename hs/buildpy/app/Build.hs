module Build where

import Config

-- processDependencies :: [DepBuildConfig] -> IO ()
-- processDependencies = mapM_ processDep
--   where
--     processDep = undefined
processPython :: PythonConfig -> IO ()
processPython b = do
    putStrLn ("building..." ++ show b)
    -- install deps
    -- download
    -- setup
    -- configure
    -- build
    -- install
    -- clean
    -- zip
