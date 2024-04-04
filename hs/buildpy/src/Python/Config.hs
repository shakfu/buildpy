{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}

module Python.Config where

import Data.List (nub)
import Data.Map (Map, delete, fromList, insert)
import System.Info (os)

import Python.Model
    ( PythonConfig(..), pythonSetupLocal, pythonVer, writeSetupLocal )
import Dependency.Config ( sslConfig, xzConfig, bz2Config )
import Project ( Project, defaultProject )
import Types
    ( Buildable(prefix, srcDir),
      SizeType(..),
      BuildType(..),
      Platform,
      Version )
import Log ( info, debug )
import Process ( cmd )

getDefault :: IO PythonConfig
getDefault = do
    p <- defaultProject
    let c = configurePython "3.12.2" Static Max p
    return c

doConfigurePython :: PythonConfig -> IO ()
doConfigurePython c = do
    debug $ "buildtype: " ++ show (pythonBuildType c)
    debug $ "config opts: " ++ show (pythonConfigOptions c)
    info "writing Setup.local"
    let file = pythonSetupLocal c
    writeSetupLocal file $ configSetupLocal c
    cmd "bash" (pythonConfigOptions c) (Just $ srcDir c) Nothing

newPythonConfig :: Version -> BuildType -> SizeType -> Project -> PythonConfig
newPythonConfig version build_type size_type proj =
    PythonConfig
        { pythonName = "Python"
        , pythonVersion = version
        , pythonBuildType = build_type
        , pythonSizeType = size_type
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
                    , [ "-lncurses"
                      , "-lncursesw"
                      , "-ltermcap"
                      , "_cursesmodule.c"
                      ])
                  , ( "_curses_panel"
                    , ["-lpanel", "-lncurses", "_curses_panel.c"])
                  , ("_datetime", ["_datetimemodule.c"])
                  , ( "_dbm"
                    , ["_dbmmodule.c", "-lgdbm_compat", "-DUSE_GDBM_COMPAT"])
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
        , pythonRemovePatterns = ["__phello__", "**/__pycache__"]
        -- , pythonRemovePatterns =
        --       [ "*.exe"
        --       , "*config-3*"
        --       , "*tcl*"
        --       , "*tdbc*"
        --       , "*tk*"
        --       , "__phello__"
        --       , "__pycache__"
        --       , "_codecs_*.so"
        --       , "_ctypes_test*"
        --       , "_test*"
        --       , "_tk*"
        --       , "_xx*.so"
        --       , "distutils"
        --       , "idlelib"
        --       , "lib2to3"
        --       , "LICENSE.txt"
        --       , "pkgconfig"
        --       , "pydoc_data"
        --       , "site-packages"
        --       , "test"
        --       , "Tk*"
        --       , "turtle*"
        --       , "venv"
        --       , "xx*.so"
        --       ]
        , pythonConfigOptions = []
        , pythonPackages = []
        , pythonDependsOn =
              [ sslConfig "1.1.1" proj
              , bz2Config "1.0.8" proj
              , xzConfig "5.2.5" proj
              ]
        , pythonOptimize = False
        , pythonProject = proj
        }

-- ----------------------------------------------------------------------------
-- methods
configurePython :: Version -> BuildType -> SizeType -> Project -> PythonConfig
configurePython version btype stype project = do
    let c = newPythonConfig version btype stype project
    let baseOpts =
            ["./configure", "--prefix=" ++ prefix c] ++ pythonConfigOptions c
    let typeOpts =
            if | pythonBuildType c == Shared ->
                   baseOpts ++ ["--enable-shared", "--without-static-libpython"]
               | pythonBuildType c == Framework ->
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

-- dropAll :: (Foldable t, Eq a) => t a -> [a] -> [a]
-- dropAll xs = filter (f xs)
--   where
--     f ys x = not $ flip elem ys x
dropAll :: (Foldable t, Eq a) => t a -> [a] -> [a]
dropAll xs = filter (f xs)
  where
    f ys x = x `notElem` ys

addAll :: [String] -> [String] -> [String]
addAll xs ys = nub $ xs ++ ys

dropFromStatic :: [String] -> PythonConfig -> PythonConfig
dropFromStatic xs c = c {pythonStatic = dropAll xs (pythonStatic c)}

addToStatic :: [String] -> PythonConfig -> PythonConfig
addToStatic xs c = c {pythonStatic = addAll xs (pythonStatic c)}

addToDisabled :: [String] -> PythonConfig -> PythonConfig
addToDisabled xs c = c {pythonDisabled = addAll xs (pythonDisabled c)}

staticToShared :: [String] -> PythonConfig -> PythonConfig
staticToShared xs c =
    c
        { pythonStatic = dropAll xs (pythonStatic c)
        , pythonShared = addAll xs (pythonShared c)
        }

staticToDisabled :: [String] -> PythonConfig -> PythonConfig
staticToDisabled xs c =
    c
        { pythonStatic = dropAll xs (pythonStatic c)
        , pythonDisabled = addAll xs (pythonDisabled c)
        }

sharedToStatic :: [String] -> PythonConfig -> PythonConfig
sharedToStatic xs c =
    c
        { pythonShared = dropAll xs (pythonShared c)
        , pythonStatic = addAll xs (pythonStatic c)
        }

sharedToDisabled :: [String] -> PythonConfig -> PythonConfig
sharedToDisabled xs c =
    c
        { pythonShared = dropAll xs (pythonShared c)
        , pythonDisabled = addAll xs (pythonDisabled c)
        }

disabledToStatic :: [String] -> PythonConfig -> PythonConfig
disabledToStatic xs c =
    c
        { pythonDisabled = dropAll xs (pythonDisabled c)
        , pythonStatic = addAll xs (pythonStatic c)
        }

disabledToShared :: [String] -> PythonConfig -> PythonConfig
disabledToShared xs c =
    c
        { pythonDisabled = dropAll xs (pythonDisabled c)
        , pythonShared = addAll xs (pythonShared c)
        }

delMapEntries :: Ord k => [k] -> Map k a -> Map k a
delMapEntries ks m = Prelude.foldl (flip delete) m ks

deleteExts :: [String] -> PythonConfig -> PythonConfig
deleteExts ks c = c {pythonExts = delMapEntries ks (pythonExts c)}

-- compose :: [a -> a] -> a -> a
compose :: Foldable t => t (b -> b) -> b -> b
compose = Prelude.foldl (flip (.)) id

-- same as 
compose' :: Foldable t => t (b -> b) -> b -> b
compose' = foldr (.) id

pipe :: Foldable t => b -> t (b -> b) -> b
pipe = foldl (flip id)

updatePythonExts :: String -> [String] -> PythonConfig -> PythonConfig
updatePythonExts k v c = c {pythonExts = insert k v $ pythonExts c}

withVersion ::
       Version -> [PythonConfig -> PythonConfig] -> PythonConfig -> PythonConfig
withVersion v fs c =
    if pythonVer c == v
        then compose fs c
        else c

withConfig ::
       BuildType
    -> SizeType
    -> [PythonConfig -> PythonConfig]
    -> PythonConfig
    -> PythonConfig
withConfig btype stype fs c =
    if (pythonBuildType c == btype) && (pythonSizeType c == stype)
        then compose fs c
        else c

withBuildType ::
       BuildType
    -> [PythonConfig -> PythonConfig]
    -> PythonConfig
    -> PythonConfig
withBuildType btype fs c =
    if pythonBuildType c == btype
        then compose fs c
        else c

withSizeType ::
       SizeType
    -> [PythonConfig -> PythonConfig]
    -> PythonConfig
    -> PythonConfig
withSizeType stype fs c =
    if pythonSizeType c == stype
        then compose fs c
        else c

withPlatform ::
       Platform
    -> [PythonConfig -> PythonConfig]
    -> PythonConfig
    -> PythonConfig
withPlatform p fs c =
    if p == os
        then compose fs c
        else c

-- ------------------------------------------------------------
-- setup.local configuration
configSetupLocal :: PythonConfig -> PythonConfig
configSetupLocal =
    compose
        -- common platform-specific
        [ withPlatform "darwin" [disabledToStatic ["_scproxy"]]
        , withPlatform
              "linux"
              [ updatePythonExts
                    "_ssl"
                    [ "_ssl.c"
                    , "-I$(OPENSSL)/include"
                    , "-L$(OPENSSL)/lib"
                    , "-l:libssl.a -Wl,--exclude-libs,libssl.a"
                    , "-l:libcrypto.a -Wl,--exclude-libs,libcrypto.a"
                    ]
              , updatePythonExts
                    "_hashlib"
                    [ "_hashopenssl.c"
                    , "-I$(OPENSSL)/include"
                    , "-L$(OPENSSL)/lib"
                    , "-l:libcrypto.a -Wl,--exclude-libs,libcrypto.a"
                    ]
              ]
        -- version-specific
        , py311
        , py312
        ]

py311 :: PythonConfig -> PythonConfig
py311 =
    withVersion
        "3.11"
        [ withConfig Static Max [staticToDisabled ["_decimal"]]
        , withConfig Static Mid [staticToDisabled ["_decimal"]]
        , withConfig
              Static
              Min
              [ staticToDisabled
                    [ "_bz2"
                    , "_decimal"
                    , "_csv"
                    , "_json"
                    , "_lzma"
                    , "_scproxy"
                    , "_sqlite3"
                    , "_ssl"
                    , "pyexpat"
                    , "readline"
                    ]
              ]
        , withConfig
              Shared
              Max
              [ withPlatform "linux" [staticToDisabled ["_decimal"]]
              , withPlatform
                    "darwin"
                    [ disabledToStatic ["_ctypes"]
                    , staticToShared ["_decimal", "_ssl", "_hashlib"]
                    ]
              ]
        , withConfig
              Shared
              Min
              [staticToDisabled ["_decimal", "_ssl", "_hashlib"]]
        ]

py312 :: PythonConfig -> PythonConfig
py312 =
    withVersion
        "3.12"
        [ updatePythonExts
              "_md5"
              [ "md5module.c"
              , "-I$(srcdir)/Modules/_hacl/include"
              , "_hacl/Hacl_Hash_MD5.c"
              , "-D_BSD_SOURCE"
              , "-D_DEFAULT_SOURCE"
              ]
        , updatePythonExts
              "_sha1"
              [ "sha1module.c"
              , "-I$(srcdir)/Modules/_hacl/include"
              , "_hacl/Hacl_Hash_SHA1.c"
              , "-D_BSD_SOURCE"
              , "-D_DEFAULT_SOURCE"
              ]
        , updatePythonExts
              "_sha2"
              [ "sha2module.c"
              , "-I$(srcdir)/Modules/_hacl/include"
              , "_hacl/Hacl_Hash_SHA2.c"
              , "-D_BSD_SOURCE"
              , "-D_DEFAULT_SOURCE"
              , "Modules/_hacl/libHacl_Hash_SHA2.a"
              ]
        , updatePythonExts
              "_sha3"
              [ "sha3module.c"
              , "-I$(srcdir)/Modules/_hacl/include"
              , "_hacl/Hacl_Hash_SHA3.c"
              , "-D_BSD_SOURCE"
              , "-D_DEFAULT_SOURCE"
              ]
        , deleteExts ["_sha256", "_sha512"]
        , addToStatic ["_sha2"]
        , addToDisabled ["_xxinterpchannels"]
        , dropFromStatic ["_sha256", "_sha512"]
        , withConfig
              Static
              Max
              [withPlatform "linux" [staticToDisabled ["_decimal"]]]
        , withConfig Static Mid [staticToDisabled ["_decimal"]]
        , withConfig
              Static
              Min
              [ staticToDisabled
                    [ "_bz2"
                    , "_decimal"
                    , "_csv"
                    , "_json"
                    , "_lzma"
                    , "_scproxy"
                    , "_sqlite3"
                    , "_ssl"
                    , "pyexpat"
                    , "readline"
                    ]
              ]
        , withConfig
              Shared
              Max
              [ disabledToShared ["_ctypes"]
              , staticToShared ["_decimal", "_ssl", "_hashlib"]
              ]
        , withConfig
              Shared
              Mid
              [staticToShared ["_decimal", "_ssl", "_hashlib"]]
        ]
