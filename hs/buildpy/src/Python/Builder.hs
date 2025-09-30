module Python.Builder where

import Project ( defaultProject )
import Python.Config ( doConfigurePython, configurePython )
import Python.Model
    ( downloadPython, setupPython, buildPython, installPython, cleanPython, zipPythonLib )
import Types

processPythonBuild :: Version -> BuildType -> SizeType -> IO ()
processPythonBuild version build_type size_type = do
    p <- defaultProject
    let c = configurePython version build_type size_type p
    setupPython c
    downloadPython c
    doConfigurePython c
    buildPython c
    installPython c
    cleanPython c
    zipPythonLib c


run :: Version -> Name -> IO ()
run v cfg
    | cfg == "static_max" = processPythonBuild v Static Max
    | cfg == "static_mid" = processPythonBuild v Static Mid
    | cfg == "static_min" = processPythonBuild v Static Min
    | cfg == "shared_max" = processPythonBuild v Shared Max
    | cfg == "shared_mid" = processPythonBuild v Shared Mid
    | cfg == "shared_min" = processPythonBuild v Shared Min
    | otherwise           = error "config not recognized"
