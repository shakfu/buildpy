module Process where

import System.Process
    ( CmdSpec(RawCommand)
    , CreateProcess(..)
    , StdStream(Inherit)
    , createProcess
    , proc
    , waitForProcess
    )

import Log (logInfo)

mkproc ::
       String
    -> [String]
    -> Maybe FilePath
    -> Maybe [(String, String)]
    -> CreateProcess
mkproc exe args wd envs =
    CreateProcess
        { cmdspec = RawCommand exe args
        , cwd = wd
        , env = envs
        , std_in = Inherit
        , std_out = Inherit
        , std_err = Inherit
        , close_fds = False
        , create_group = False
        , delegate_ctlc = False
        , detach_console = False
        , create_new_console = False
        , new_session = False
        , child_group = Nothing
        , child_user = Nothing
        , use_process_jobs = False
        }

run :: String -> [String] -> IO ()
run exe args = do
    (_, _, _, p1) <- createProcess (proc exe args)
    logInfo (exe ++ " " ++ unwords args ++ " DONE")
    _ <- waitForProcess p1
    return ()

cmd :: String -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> IO ()
cmd exe args wd envs = do
    (_, _, _, p1) <- createProcess (mkproc exe args wd envs)
    logInfo (exe ++ " " ++ unwords args ++ " DONE")
    _ <- waitForProcess p1
    return ()
