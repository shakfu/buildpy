{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE InstanceSigs #-}

module Models.Python where

import Control.Monad
import Data.Map
import Data.Maybe
import System.FilePath (joinPath)

import Log
import Models.Dependency
import Models.Project
import Process
import Shell
import Types
import Utils

data PythonConfig = PythonConfig
  { pythonName :: Name
  , pythonVersion :: Version
  , pythonConfig :: Name
  , pythonRepoUrl :: Url
  , pythonRepoBranch :: String
  , pythonDownloadUrl :: Url
  , pythonHeaders :: [String]
  , pythonExts :: Map String [String]
  , pythonCore :: [String]
  , pythonStatic :: [String]
  , pythonShared :: [String]
  , pythonDisabled :: [String]
  , pythonRemovePatterns :: [String]
  , pythonConfigOptions :: [String]
  , pythonPackages :: [String]
  , pythonDependsOn :: [Dependency]
  , pythonOptimize :: Bool
  , pythonProject :: Project
  } deriving (Show)

newPythonConfig :: Version -> Name -> Project -> PythonConfig
newPythonConfig version name proj =
  PythonConfig
    { pythonName = "Python"
    , pythonVersion = version
    , pythonConfig = name
    , pythonRepoUrl = "https://github.com/python/cpython.git"
    , pythonRepoBranch = "v" ++ version
    , pythonDownloadUrl =
        "https://www.python.org/ftp/python/"
          ++ version
          ++ "/Python-"
          ++ version
          ++ ".tar.xz"
    , pythonHeaders =
        [ "DESTLIB=$(LIBDEST)"
        , "MACHDESTLIB=$(BINLIBDEST)"
        , "DESTPATH="
        , "SITEPATH="
        , "TESTPATH="
        , "COREPYTHONPATH=$(DESTPATH)$(SITEPATH)$(TESTPATH)"
        , "PYTHONPATH=$(COREPYTHONPATH)"
        , "OPENSSL=$(srcdir)/../../install/openssl"
        , "BZIP2=$(srcdir)/../../install/bzip2"
        , "LZMA=$(srcdir)/../../install/xz"
        ]
    , pythonExts =
        fromList
          [ ("name", ["name", "dest"])
          , ("_abc", ["_abc.c"])
          , ("_asyncio", ["_asynciomodule.c"])
          , ("_bisect", ["_bisectmodule.c"])
          , ( "_blake2"
            , [ "_blake2/blake2module.c"
              , "_blake2/blake2b_impl.c"
              , "_blake2/blake2s_impl.c"
              ])
          , ( "_bz2"
            , [ "_bz2module.c"
              , "-I$(BZIP2)/include"
              , "-L$(BZIP2)/lib"
              , "$(BZIP2)/lib/libbz2.a"
              ])
          , ("_codecs", ["_codecsmodule.c"])
          , ("_codecs_cn", ["cjkcodecs/_codecs_cn.c"])
          , ("_codecs_hk", ["cjkcodecs/_codecs_hk.c"])
          , ("_codecs_iso2022", ["cjkcodecs/_codecs_iso2022.c"])
          , ("_codecs_jp", ["cjkcodecs/_codecs_jp.c"])
          , ("_codecs_kr", ["cjkcodecs/_codecs_kr.c"])
          , ("_codecs_tw", ["cjkcodecs/_codecs_tw.c"])
          , ("_collections", ["_collectionsmodule.c"])
          , ("_contextvars", ["_contextvarsmodule.c"])
          , ("_csv", ["_csv.c"])
          , ( "_ctypes"
            , [ "_ctypes/_ctypes.c"
              , "_ctypes/callbacks.c"
              , "_ctypes/callproc.c"
              , "_ctypes/stgdict.c"
              , "_ctypes/cfield.c"
              , "-ldl"
              , "-lffi"
              , "-DHAVE_FFI_PREP_CIF_VAR"
              , "-DHAVE_FFI_PREP_CLOSURE_LOC"
              , "-DHAVE_FFI_CLOSURE_ALLOC"
              ])
          , ( "_curses"
            , ["-lncurses", "-lncursesw", "-ltermcap", "_cursesmodule.c"])
          , ("_curses_panel", ["-lpanel", "-lncurses", "_curses_panel.c"])
          , ("_datetime", ["_datetimemodule.c"])
          , ("_dbm", ["_dbmmodule.c", "-lgdbm_compat", "-DUSE_GDBM_COMPAT"])
          , ("_decimal", ["_decimal/_decimal.c", "-DCONFIG_64=1"])
          , ("_elementtree", ["_elementtree.c"])
          , ( "_functools"
            , [ "-DPy_BUILD_CORE_BUILTIN"
              , "-I$(srcdir)/Include/internal"
              , "_functoolsmodule.c"
              ])
          , ("_gdbm", ["_gdbmmodule.c", "-lgdbm"])
          , ( "_hashlib"
            , [ "_hashopenssl.c"
              , "-I$(OPENSSL)/include"
              , "-L$(OPENSSL)/lib"
              , "$(OPENSSL)/lib/libcrypto.a"
              ])
          , ("_heapq", ["_heapqmodule.c"])
          , ( "_io"
            , [ "_io/_iomodule.c"
              , "_io/iobase.c"
              , "_io/fileio.c"
              , "_io/bytesio.c"
              , "_io/bufferedio.c"
              , "_io/textio.c"
              , "_io/stringio.c"
              ])
          , ("_json", ["_json.c"])
          , ("_locale", ["-DPy_BUILD_CORE_BUILTIN", "_localemodule.c"])
          , ("_lsprof", ["_lsprof.c", "rotatingtree.c"])
          , ( "_lzma"
            , [ "_lzmamodule.c"
              , "-I$(LZMA)/include"
              , "-L$(LZMA)/lib"
              , "$(LZMA)/lib/liblzma.a"
              ])
          , ("_md5", ["md5module.c"])
          , ("_multibytecodec", ["cjkcodecs/multibytecodec.c"])
          , ( "_multiprocessing"
            , [ "_multiprocessing/multiprocessing.c"
              , "_multiprocessing/semaphore.c"
              ])
          , ("_opcode", ["_opcode.c"])
          , ("_operator", ["_operator.c"])
          , ("_pickle", ["_pickle.c"])
          , ("_posixshmem", ["_multiprocessing/posixshmem.c"])
          , ("_posixsubprocess", ["_posixsubprocess.c"])
          , ("_queue", ["_queuemodule.c"])
          , ("_random", ["_randommodule.c"])
          , ("_scproxy", ["_scproxy.c"])
          , ("_sha1", ["sha1module.c"])
          , ("_sha256", ["sha256module.c"])
          , ("_sha3", ["_sha3/sha3module.c"])
          , ("_sha512", ["sha512module.c"])
          , ( "_signal"
            , [ "-DPy_BUILD_CORE_BUILTIN"
              , "-I$(srcdir)/Include/internal"
              , "signalmodule.c"
              ])
          , ("_socket", ["socketmodule.c"])
          , ( "_sqlite3"
            , [ "_sqlite/blob.c"
              , "_sqlite/connection.c"
              , "_sqlite/cursor.c"
              , "_sqlite/microprotocols.c"
              , "_sqlite/module.c"
              , "_sqlite/prepare_protocol.c"
              , "_sqlite/row.c"
              , "_sqlite/statement.c"
              , "_sqlite/util.c"
              ])
          , ("_sre", ["_sre/sre.c", "-DPy_BUILD_CORE_BUILTIN"])
          , ( "_ssl"
            , [ "_ssl.c"
              , "-I$(OPENSSL)/include"
              , "-L$(OPENSSL)/lib"
              , "$(OPENSSL)/lib/libcrypto.a"
              , "$(OPENSSL)/lib/libssl.a"
              ])
          , ("_stat", ["_stat.c"])
          , ("_statistics", ["_statisticsmodule.c"])
          , ("_struct", ["_struct.c"])
          , ("_symtable", ["symtablemodule.c"])
          , ( "_thread"
            , [ "-DPy_BUILD_CORE_BUILTIN"
              , "-I$(srcdir)/Include/internal"
              , "_threadmodule.c"
              ])
          , ("_tracemalloc", ["_tracemalloc.c"])
          , ("_typing", ["_typingmodule.c"])
          , ("_uuid", ["_uuidmodule.c"])
          , ("_weakref", ["_weakref.c"])
          , ("_zoneinfo", ["_zoneinfo.c"])
          , ("array", ["arraymodule.c"])
          , ("atexit", ["atexitmodule.c"])
          , ("binascii", ["binascii.c"])
          , ("cmath", ["cmathmodule.c"])
          , ("errno", ["errnomodule.c"])
          , ("faulthandler", ["faulthandler.c"])
          , ("fcntl", ["fcntlmodule.c"])
          , ("grp", ["grpmodule.c"])
          , ("itertools", ["itertoolsmodule.c"])
          , ("math", ["mathmodule.c"])
          , ("mmap", ["mmapmodule.c"])
          , ("ossaudiodev", ["ossaudiodev.c"])
          , ( "posix"
            , [ "-DPy_BUILD_CORE_BUILTIN"
              , "-I$(srcdir)/Include/internal"
              , "posixmodule.c"
              ])
          , ("pwd", ["pwdmodule.c"])
          , ( "pyexpat"
            , [ "expat/xmlparse.c"
              , "expat/xmlrole.c"
              , "expat/xmltok.c"
              , "pyexpat.c"
              , "-I$(srcdir)/Modules/expat"
              , "-DHAVE_EXPAT_CONFIG_H"
              , "-DUSE_PYEXPAT_CAPI"
              , "-DXML_DEV_URANDOM"
              ])
          , ("readline", ["readline.c", "-lreadline", "-ltermcap"])
          , ("resource", ["resource.c"])
          , ("select", ["selectmodule.c"])
          , ("spwd", ["spwdmodule.c"])
          , ("syslog", ["syslogmodule.c"])
          , ("termios", ["termios.c"])
          , ( "time"
            , [ "-DPy_BUILD_CORE_BUILTIN"
              , "-I$(srcdir)/Include/internal"
              , "timemodule.c"
              ])
          , ("unicodedata", ["unicodedata.c"])
          , ("zlib", ["zlibmodule.c", "-lz"])
          ]
    , pythonCore =
        [ "_abc"
        , "_codecs"
        , "_collections"
        , "_functools"
        , "_io"
        , "_locale"
        , "_operator"
        , "_signal"
        , "_sre"
        , "_stat"
        , "_symtable"
        , "_thread"
        , "_tracemalloc"
        , "_weakref"
        , "atexit"
        , "errno"
        , "faulthandler"
        , "itertools"
        , "posix"
        , "pwd"
        , "time"
        ]
    , pythonStatic =
        [ "_asyncio"
        , "_bisect"
        , "_blake2"
        , "_bz2"
        , "_contextvars"
        , "_csv"
        , "_datetime"
        , "_decimal"
        , "_elementtree"
        , "_hashlib"
        , "_heapq"
        , "_json"
        , "_lsprof"
        , "_lzma"
        , "_md5"
        , "_multibytecodec"
        , "_multiprocessing"
        , "_opcode"
        , "_pickle"
        , "_posixshmem"
        , "_posixsubprocess"
        , "_queue"
        , "_random"
        , "_sha1"
        , "_sha256"
        , "_sha3"
        , "_sha512"
        , "_socket"
        , "_sqlite3"
        , "_ssl"
        , "_statistics"
        , "_struct"
        , "_typing"
        , "_uuid"
        , "_zoneinfo"
        , "array"
        , "binascii"
        , "cmath"
        , "fcntl"
        , "grp"
        , "math"
        , "mmap"
        , "pyexpat"
        , "readline"
        , "select"
        , "unicodedata"
        , "zlib"
        ]
    , pythonShared = []
    , pythonDisabled =
        [ "_codecs_cn"
        , "_codecs_hk"
        , "_codecs_iso2022"
        , "_codecs_jp"
        , "_codecs_kr"
        , "_codecs_tw"
        , "_crypt"
        , "_ctypes"
        , "_curses"
        , "_curses_panel"
        , "_dbm"
        , "_scproxy"
        , "_tkinter"
        , "_xxsubinterpreters"
        , "audioop"
        , "nis"
        , "ossaudiodev"
        , "resource"
        , "spwd"
        , "syslog"
        , "termios"
        , "xxlimited"
        , "xxlimited_35"
        ]
    , pythonRemovePatterns =
        [ "*.exe"
        , "*config-3*"
        , "*tcl*"
        , "*tdbc*"
        , "*tk*"
        , "__phello__"
        , "__pycache__"
        , "_codecs_*.so"
        , "_ctypes_test*"
        , "_test*"
        , "_tk*"
        , "_xx*.so"
        , "distutils"
        , "idlelib"
        , "lib2to3"
        , "LICENSE.txt"
        , "pkgconfig"
        , "pydoc_data"
        , "site-packages"
        , "test"
        , "Tk*"
        , "turtle*"
        , "venv"
        , "xx*.so"
        ]
    , pythonConfigOptions = []
    , pythonPackages = []
    , pythonDependsOn =
        [sslConfig "1.1.1" proj, bz2Config "1.0.8" proj, xzConfig "5.6.0" proj]
    , pythonOptimize = False
    , pythonProject = proj
    }

-- ----------------------------------------------------------------------------
-- properties

instance Buildable PythonConfig where

  prefix :: PythonConfig -> FilePath
  prefix c = joinPath [projectInstall p, lowercase $ pythonName c]
    where
      p = pythonProject c

  srcDir :: PythonConfig -> FilePath
  srcDir c = joinPath [projectSrc p, lowercase $ pythonName c]
    where
      p = pythonProject c

  buildDir :: PythonConfig -> FilePath
  buildDir c = joinPath [srcDir c, "build"]

  download :: PythonConfig -> IO ()
  download c = do
    Shell.gitClone url branch dir False
    where
      url = pythonRepoUrl c
      branch = pythonRepoBranch c
      dir = srcDir c

  build :: PythonConfig -> IO ()
  build c   = do
    info ("bui  lding python " ++ show (pythonVersion c))
    cmd "make" [  ] (Just $ srcDir c) Nothing


pythonBuildType :: PythonConfig -> String
pythonBuildType c = head $ wordsWhen (== '_') (pythonConfig c)

pythonSetupLocal :: PythonConfig -> FilePath
pythonSetupLocal c = joinPath [srcDir c, "Modules", "Setup.local"]

-- ----------------------------------------------------------------------------
-- methods
configurePython :: Version -> Name -> Project -> PythonConfig
configurePython version name project = do
  let c = newPythonConfig version name project
  let baseOpts =
        ["./configure", "--prefix=" ++ prefix c] ++ pythonConfigOptions c
  let typeOpts =
        if | pythonBuildType c == "shared" ->
             baseOpts ++ ["--enable-shared", "--without-static-libpython"]
           | pythonBuildType c == "framework" ->
             baseOpts ++ ["--enable-framework=" ++ prefix c]
           | otherwise -> baseOpts
  let opts =
        if pythonOptimize c
          then typeOpts ++ ["--enable-optimizations"]
          else typeOpts
  let nopackages = Prelude.null (pythonPackages c)
  c
    { pythonConfigOptions =
        if nopackages
          then opts ++ ["--without-ensurepip"]
          else opts
    , pythonRemovePatterns =
        if nopackages
          then pythonRemovePatterns c ++ ["ensurepip"]
          else pythonRemovePatterns c
    }

writeSetupLocal :: PythonConfig -> IO ()
writeSetupLocal c = do
  let out =
        ["# -*- makefile -*-"]
          ++ pythonHeaders c
          ++ ["\n# core\n"]
          ++ getEntries (pythonCore c)
          ++ ["\n*shared*\n"]
          ++ getEntries (pythonShared c)
          ++ ["\n*static*\n"]
          ++ getEntries (pythonStatic c)
          ++ ["\n*disabled*\n"]
          ++ pythonDisabled c
  writeFile (pythonSetupLocal c) (unlines out)
  where
    extlookup k = k : fromJust (Data.Map.lookup k (pythonExts c))
    getEntries = Prelude.map (unwords . extlookup)

doConfigurePython :: PythonConfig -> IO ()
doConfigurePython c = do
  --     let mut cfg = config::Config::new(self.config.clone(), self.ver());
  --     cfg.configure();
  debug $ "buildtype: " ++ pythonBuildType c
  debug $ "config opts: " ++ show (pythonConfigOptions c)
  info "writing Setup.local"
  writeSetupLocal c
  cmd "bash" (pythonConfigOptions c) (Just $ srcDir c) Nothing

processPython :: IO ()
processPython = do
  p <- defaultProject
  let c = configurePython "3.12.2" "static_max" p
  setupProject $ pythonProject c
  processPythonDependencies c
  downloadPython c
  doConfigurePython c
  buildPython c
  installPython c
  -- cleanPython c
  -- zipPython c

processPythonDependencies :: PythonConfig -> IO ()
processPythonDependencies c = do
  forM_ (pythonDependsOn c) $ \dep -> do
    depBuildFunc dep dep

downloadPython :: PythonConfig -> IO ()
downloadPython c = do
  Shell.gitClone url branch dir False
  where
    url = pythonRepoUrl c
    branch = pythonRepoBranch c
    dir = srcDir c

-- setupPython :: PythonConfig -> IO ()
-- setupPython c = do
--   print c
buildPython :: PythonConfig -> IO ()
buildPython c = do
  info ("building python " ++ show (pythonVersion c))
  cmd "make" [] (Just $ srcDir c) Nothing

installPython :: PythonConfig -> IO ()
installPython c = do
  info ("install python " ++ show (pythonVersion c))
  cmd "make" ["install"] (Just $ srcDir c) Nothing

cleanPython :: PythonConfig -> IO ()
cleanPython c = do
  print c

zipPython :: PythonConfig -> IO ()
zipPython c = do
  print c
