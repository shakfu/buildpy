module Config 
    (configName, defaultConfig)
    where

data BuildConfig = BuildConfig
    { configName       :: String
    , configVersion    :: String
    , configHeaders    :: [String]
    , configCore       :: [String]
    , configStatic     :: [String]
    , configShared     :: [String]
    , configDisabled   :: [String]
    } deriving Show


defaultConfig :: BuildConfig
defaultConfig = BuildConfig 
    { configName = "static_max"
    , configVersion = "2.1.2"
    , configHeaders = [
            "DESTLIB=$(LIBDEST)",
            "MACHDESTLIB=$(BINLIBDEST)",
            "DESTPATH=",
            "SITEPATH=",
            "TESTPATH=",
            "COREPYTHONPATH=$(DESTPATH)$(SITEPATH)$(TESTPATH)",
            "PYTHONPATH=$(COREPYTHONPATH)",
            "OPENSSL=$(srcdir)/../../install/openssl",
            "BZIP2=$(srcdir)/../../install/bzip2",
            "LZMA=$(srcdir)/../../install/xz"
        ]
    , configCore = [
            "_abc",
            "_codecs",
            "_collections",
            "_functools",
            "_io",
            "_locale",
            "_operator",
            "_signal",
            "_sre",
            "_stat",
            "_symtable",
            "_thread",
            "_tracemalloc",
            "_weakref",
            "atexit",
            "errno",
            "faulthandler",
            "itertools",
            "posix",
            "pwd",
            "time"        
        ]
    ,   configStatic = []
    ,   configShared = []
    ,   configDisabled = []
    }
