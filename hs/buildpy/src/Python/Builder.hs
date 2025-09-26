module Python.Builder where

import Project ( defaultProject )
import Python.Config ( doConfigurePython, configurePython )
import Python.Model
    ( downloadPython, setupPython, buildPython, installPython, cleanPython, zipPythonLib )
import Types

processPython :: Version -> BuildType -> SizeType -> IO ()
processPython version build_type size_type = do
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
    | cfg == "static_max" = processPython v Static Max
    | cfg == "static_mid" = processPython v Static Mid
    | cfg == "static_min" = processPython v Static Min
    | cfg == "shared_max" = processPython v Shared Max
    | cfg == "shared_mid" = processPython v Shared Mid
    | cfg == "shared_min" = processPython v Shared Min
    | otherwise           = error "config not recognized"
