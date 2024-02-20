HEADER = [
    "DESTLIB=$(LIBDEST)",
    "MACHDESTLIB=$(BINLIBDEST)",
    "DESTPATH=",
    "SITEPATH=",
    "TESTPATH=",
    "COREPYTHONPATH=$(DESTPATH)$(SITEPATH)$(TESTPATH)",
    "PYTHONPATH=$(COREPYTHONPATH)",

    "OPENSSL=$(srcdir)/../../lib/openssl",
    "OPENSSL_INCLUDES=-I$(OPENSSL)/include",
    "OPENSSL_LDFLAGS=-L$(OPENSSL)/lib",
    "BZIP2=$(srcdir)/../../lib/bzip2",
    "BZIP2_INCLUDES=-I$(BZIP2)/include",
    "BZIP2_LDFLAGS=-L$(BZIP2)/lib",
    "LZMA=$(srcdir)/../../lib/xz",
    "LZMA_INCLUDES=-I$(LZMA)/include",
    "LZMA_LDFLAGS=-L$(LZMA)/lib",

    "\n# core extensions\n"
]

EXTENSIONS = {
    "_abc": ["_abc.c"],
    "_asyncio": ["_asynciomodule.c"],
    "_bisect": ["_bisectmodule.c"],
    "_bz2": [
        "_bz2module.c",
        "$(BZIP2_INCLUDES)",
        "$(BZIP2_LDFLAGS)",
        "$(BZIP2)/lib/libbz2.a",
    ],
    "_codecs": ["_codecsmodule.c"],
    "_collections": ["_collectionsmodule.c"],
    "_contextvars": ["_contextvarsmodule.c"],
    "_csv": ["_csv.c"],
    "_datetime": ["_datetimemodule.c"],
    "_decimal": [
        "_decimal/_decimal.c",
        "_decimal/libmpdec/basearith.c",
        "_decimal/libmpdec/constants.c",
        "_decimal/libmpdec/context.c",
        "_decimal/libmpdec/convolute.c",
        "_decimal/libmpdec/crt.c",
        "_decimal/libmpdec/difradix2.c",
        "_decimal/libmpdec/fnt.c",
        "_decimal/libmpdec/fourstep.c",
        "_decimal/libmpdec/io.c",
        "_decimal/libmpdec/memory.c",
        "_decimal/libmpdec/mpdecimal.c",
        "_decimal/libmpdec/numbertheory.c",
        "_decimal/libmpdec/sixstep.c",
        "_decimal/libmpdec/transpose.c",
        "-I$(srcdir)/Modules/_decimal/libmpdec",
        "-DCONFIG_64=1",
        "-DANSI=1",
        "-DHAVE_UINT128_T=1",
    ],
    "_elementtree": ["_elementtree.c"],
    "_functools": [
        "-DPy_BUILD_CORE_BUILTIN",
        "-I$(srcdir)/Include/internal",
        "_functoolsmodule.c",
    ],
    "_hashlib": [
        "_hashopenssl.c",
        "$(OPENSSL_INCLUDES)",
        "$(OPENSSL_LDFLAGS)",
        "$(OPENSSL)/lib/libcrypto.a",
    ],
    "_heapq": ["_heapqmodule.c"],
    "_io": [
        "_io/_iomodule.c",
        "_io/iobase.c",
        "_io/fileio.c",
        "_io/bytesio.c",
        "_io/bufferedio.c",
        "_io/textio.c",
        "_io/stringio.c",
    ],
    "_json": ["_json.c"],
    "_locale": ["-DPy_BUILD_CORE_BUILTIN", "_localemodule.c"],
    "_lsprof": ["_lsprof.c", "rotatingtree.c"],
    "_lzma": [
        "_lzmamodule.c",
        "$(LZMA_INCLUDES)",
        "$(LZMA_LDFLAGS)",
        "$(LZMA)/lib/liblzma.a",
    ],
    "_md5": ["md5module.c"],
    "_multibytecodec": ["cjkcodecs/multibytecodec.c"],
    "_multiprocessing": [
        "_multiprocessing/multiprocessing.c",
        "_multiprocessing/semaphore.c",
    ],
    "_opcode": ["_opcode.c"],
    "_operator": ["_operator.c"],
    "_pickle": ["_pickle.c"],
    "_posixshmem": ["_multiprocessing/posixshmem.c"],
    "_posixsubprocess": ["_posixsubprocess.c"],
    "_queue": ["_queuemodule.c"],
    "_random": ["_randommodule.c"],
    "_sha1": ["sha1module.c"],
    "_sha256": ["sha256module.c"],
    "_sha3": ["_sha3/sha3module.c"],
    "_sha512": ["sha512module.c"],
    "_signal": [
        "-DPy_BUILD_CORE_BUILTIN",
        "-I$(srcdir)/Include/internal",
        "signalmodule.c",
    ],
    "_socket": ["socketmodule.c"],
    "_sre": ["_sre/sre.c", "-DPy_BUILD_CORE_BUILTIN"],
    "_ssl": [
        "_ssl.c",
        "$(OPENSSL_INCLUDES)",
        "$(OPENSSL_LDFLAGS)",
        "$(OPENSSL)/lib/libcrypto.a",
        "$(OPENSSL)/lib/libssl.a",
    ],
    "_stat": ["_stat.c"],
    "_statistics": ["_statisticsmodule.c"],
    "_struct": ["_struct.c"],
    "_symtable": ["symtablemodule.c"],
    "_thread": [
        "-DPy_BUILD_CORE_BUILTIN",
        "-I$(srcdir)/Include/internal",
        "_threadmodule.c",
    ],
    "_tracemalloc": ["_tracemalloc.c"],
    "_typing": ["_typingmodule.c"],
    "_uuid": ["_uuidmodule.c"],
    "_weakref": ["_weakref.c"],
    "array": ["arraymodule.c"],
    "atexit": ["atexitmodule.c"],
    "binascii": ["binascii.c"],
    "cmath": ["cmathmodule.c"],
    "errno": ["errnomodule.c"],
    "faulthandler": ["faulthandler.c"],
    "fcntl": ["fcntlmodule.c"],
    "grp": ["grpmodule.c"],
    "itertools": ["itertoolsmodule.c"],
    "math": ["mathmodule.c"],
    "mmap": ["mmapmodule.c"],
    "posix": [
        "-DPy_BUILD_CORE_BUILTIN",
        "-I$(srcdir)/Include/internal",
        "posixmodule.c",
    ],
    "pwd": ["pwdmodule.c"],
    "pyexpat": [
        "expat/xmlparse.c",
        "expat/xmlrole.c",
        "expat/xmltok.c",
        "pyexpat.c",
        "-I$(srcdir)/Modules/expat",
        "-DHAVE_EXPAT_CONFIG_H",
        "-DUSE_PYEXPAT_CAPI",
        "-DXML_DEV_URANDOM",
    ],
    "select": ["selectmodule.c"],
    "time": ["-DPy_BUILD_CORE_BUILTIN", "-I$(srcdir)/Include/internal", "timemodule.c"],
    "unicodedata": ["unicodedata.c"],
    "zlib": ["zlibmodule.c", "-lz"],
}

CORE = [
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
    "time",
]

static_local = dict(
    name='static.local',
    static=[
        "_asyncio",
        "_bisect",
        "_bz2",
        "_contextvars",
        "_csv",
        "_datetime",
        "_decimal",
        "_elementtree",
        "_hashlib",
        "_heapq",
        "_json",
        "_lsprof",
        "_lzma",
        "_md5",
        "_multibytecodec",
        "_multiprocessing",
        "_opcode",
        "_pickle",
        "_posixshmem",
        "_posixsubprocess",
        "_queue",
        "_random",
        "_sha1",
        "_sha256",
        "_sha3",
        "_sha512",
        "_socket",
        "_ssl",
        "_statistics",
        "_struct",
        "_typing",
        "_uuid",
        "array",
        "binascii",
        "cmath",
        "fcntl",
        "grp",
        "math",
        "mmap",
        "pyexpat",
        "select",
        "unicodedata",
        "zlib",
    ],
    disabled=[
        "_blake2",
        "_codecs_cn",
        "_codecs_hk",
        "_codecs_iso2022",
        "_codecs_jp",
        "_codecs_kr",
        "_codecs_tw",
        "_crypt",
        "_ctypes",
        "_curses",
        "_curses_panel",
        "_dbm",
        "_scproxy",
        "_sqlite3",
        "_tkinter",
        "_xxsubinterpreters",
        "_zoneinfo",
        "audioop",
        "nis",
        "readline",
        "resource",
        "syslog",
        "termios",
        "xxlimited",
        "xxlimited_35",
    ],
)

class Config:
    def __init__(self, cfg: dict):
        self.cfg = cfg
        self.out = HEADER.copy()

    def add_section(self, name):
        if name in self.cfg:
            self.out.append(f"\n*{name}*\n")
            for i in self.cfg[name]:
                if name == 'disabled':
                    line = [i]
                else:
                    ext = EXTENSIONS[i]
                    line = [i] + ext
                self.out.append(" ".join(line))

    def write(self):
        for i in CORE:
            ext = EXTENSIONS[i]
            line = [i] + ext
            self.out.append(" ".join(line))
        for section in ['shared', 'static', 'disabled']:
            self.add_section(section)

        with open(self.cfg['name'], 'w') as f:
            f.write("\n".join(self.out))

if __name__ == '__main__':
    cfg = Config(static_local)
    cfg.write()
