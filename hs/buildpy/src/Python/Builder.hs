module Python.Builder where

import Project ( defaultProject )
import Python.Config ( doConfigurePython, configurePython )
import Python.Model
    ( downloadPython, setupPython, buildPython, installPython )
import Types ( SizeType(Max), BuildType(Static) )

processPython :: IO ()
processPython = do
    p <- defaultProject
    let c = configurePython "3.12.2" Static Max p
    setupPython c
    downloadPython c
    doConfigurePython c
    buildPython c
    installPython c
    -- cleanPython c
    -- zipPythonLib c
