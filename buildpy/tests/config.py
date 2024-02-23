import os
import copy


class Config:
    def __init__(self, cfg: dict):
        self.cfg = cfg.copy()
        self.out = ["# -*- makefile -*-"] + self.cfg['header'] + ["\n# core\n"]

    def disable_static(self, *names):
        for name in names:
            self.cfg['static'].remove(name)
            self.cfg['disabled'].append(name)

    def disable_shared(self, *names):
        for name in names:
            self.cfg['shared'].remove(name)
            self.cfg['disabled'].append(name)

    def add_section(self, name):
        if self.cfg[name]:
            self.out.append(f"\n*{name}*\n")
            for i in sorted(self.cfg[name]):
                if name == "disabled":
                    line = [i]
                else:
                    ext = self.cfg['extensions'][i]
                    line = [i] + ext
                self.out.append(" ".join(line))

    def write(self, name):
        for i in self.cfg['core']:
            ext = self.cfg['extensions'][i]
            line = [i] + ext
            self.out.append(" ".join(line))
        for section in ["shared", "static", "disabled"]:
            self.add_section(section)

        target = os.path.join('patch', name)
        with open(target, "w") as f:
            f.write("\n".join(self.out))

    def clone(self):
        return copy.copy(self)


BASE_CONFIG = Config(cfg=dict(
        header = [
            "DESTLIB=$(LIBDEST)",
            "MACHDESTLIB=$(BINLIBDEST)",
            "DESTPATH=",
            "SITEPATH=",
            "TESTPATH=",
            "COREPYTHONPATH=$(DESTPATH)$(SITEPATH)$(TESTPATH)",
            "PYTHONPATH=$(COREPYTHONPATH)",
            "OPENSSL=$(srcdir)/../../install/openssl",
            "BZIP2=$(srcdir)/../../install/bzip2",
            "LZMA=$(srcdir)/../../install/xz",
        ],

        extensions = {
            "_abc": ["_abc.c"],
            "_asyncio": ["_asynciomodule.c"],
            "_bisect": ["_bisectmodule.c"],
            "_blake2": ["_blake2/blake2module.c", "_blake2/blake2b_impl.c", "_blake2/blake2s_impl.c"],
            "_bz2": ["_bz2module.c", "-I$(BZIP2)/include", "-L$(BZIP2)/lib", "$(BZIP2)/lib/libbz2.a"],
            "_codecs": ["_codecsmodule.c"],
            "_codecs_cn": ["cjkcodecs/_codecs_cn.c"], 
            "_codecs_hk": ["cjkcodecs/_codecs_hk.c"], 
            "_codecs_iso2022": ["cjkcodecs/_codecs_iso2022.c"], 
            "_codecs_jp": ["cjkcodecs/_codecs_jp.c"],
            "_codecs_kr": ["cjkcodecs/_codecs_kr.c"],
            "_codecs_tw": ["cjkcodecs/_codecs_tw.c"],
            "_collections": ["_collectionsmodule.c"],
            "_contextvars": ["_contextvarsmodule.c"],
            "_csv": ["_csv.c"],
            "_ctypes": ["_ctypes/_ctypes.c", "_ctypes/callbacks.c", "_ctypes/callproc.c", "_ctypes/stgdict.c", "_ctypes/cfield.c", "-ldl", "-lffi", "-DHAVE_FFI_PREP_CIF_VAR", "-DHAVE_FFI_PREP_CLOSURE_LOC", "-DHAVE_FFI_CLOSURE_ALLOC"],
            "_curses": ["-lncurses", "-lncursesw", "-ltermcap", "_cursesmodule.c"],
            "_curses_panel": ["-lpanel", "-lncurses", "_curses_panel.c"],
            "_datetime": ["_datetimemodule.c"],
            "_dbm": ["_dbmmodule.c", "-lgdbm_compat", "-DUSE_GDBM_COMPAT"],
            "_decimal": ["_decimal/_decimal.c", "-DCONFIG_64=1"],
            # "_decimal": ["_decimal/_decimal.c", "$(srcdir)/Modules/_decimal/libmpdec/libmpdec.a", "-I$(srcdir)/Modules/_decimal/libmpdec", "-DCONFIG_64=1", "-DANSI=1", "-DHAVE_UINT128_T=1"],
            "_elementtree": ["_elementtree.c"],
            "_functools": ["-DPy_BUILD_CORE_BUILTIN", "-I$(srcdir)/Include/internal", "_functoolsmodule.c"],
            "_gdbm": ["_gdbmmodule.c", "-lgdbm"],
            "_hashlib": ["_hashopenssl.c", "-I$(OPENSSL)/include", "-L$(OPENSSL)/lib", "$(OPENSSL)/lib/libcrypto.a"],
            "_heapq": ["_heapqmodule.c"],
            "_io": ["_io/_iomodule.c", "_io/iobase.c", "_io/fileio.c", "_io/bytesio.c", "_io/bufferedio.c", "_io/textio.c", "_io/stringio.c"],
            "_json": ["_json.c"],
            "_locale": ["-DPy_BUILD_CORE_BUILTIN", "_localemodule.c"],
            "_lsprof": ["_lsprof.c", "rotatingtree.c"],
            "_lzma": ["_lzmamodule.c", "-I$(LZMA)/include", "-L$(LZMA)/lib", "$(LZMA)/lib/liblzma.a"],
            "_md5": ["md5module.c"],
            "_multibytecodec": ["cjkcodecs/multibytecodec.c"],
            "_multiprocessing": ["_multiprocessing/multiprocessing.c", "_multiprocessing/semaphore.c"],
            "_opcode": ["_opcode.c"],
            "_operator": ["_operator.c"],
            "_pickle": ["_pickle.c"],
            "_posixshmem": ["_multiprocessing/posixshmem.c"],
            "_posixsubprocess": ["_posixsubprocess.c"],
            "_queue": ["_queuemodule.c"],
            "_random": ["_randommodule.c"],
            "_scproxy": ["_scproxy.c"],
            "_sha1": ["sha1module.c"],
            "_sha256": ["sha256module.c"],
            "_sha3": ["_sha3/sha3module.c"],
            "_sha512": ["sha512module.c"],
            "_signal": ["-DPy_BUILD_CORE_BUILTIN", "-I$(srcdir)/Include/internal", "signalmodule.c"],
            "_socket": ["socketmodule.c"],
            "_sqlite3": ["_sqlite/blob.c", "_sqlite/connection.c", "_sqlite/cursor.c", "_sqlite/microprotocols.c", "_sqlite/module.c", "_sqlite/prepare_protocol.c", "_sqlite/row.c", "_sqlite/statement.c", "_sqlite/util.c"],
            "_sre": ["_sre/sre.c", "-DPy_BUILD_CORE_BUILTIN"],
            "_ssl": ["_ssl.c", "-I$(OPENSSL)/include", "-L$(OPENSSL)/lib", "$(OPENSSL)/lib/libcrypto.a", "$(OPENSSL)/lib/libssl.a"],
            "_stat": ["_stat.c"],
            "_statistics": ["_statisticsmodule.c"],
            "_struct": ["_struct.c"],
            "_symtable": ["symtablemodule.c"],
            "_thread": ["-DPy_BUILD_CORE_BUILTIN", "-I$(srcdir)/Include/internal", "_threadmodule.c"],
            "_tracemalloc": ["_tracemalloc.c"],
            "_typing": ["_typingmodule.c"],
            "_uuid": ["_uuidmodule.c"],
            "_weakref": ["_weakref.c"],
            "_zoneinfo": ["_zoneinfo.c"],
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
            "posix": ["-DPy_BUILD_CORE_BUILTIN", "-I$(srcdir)/Include/internal", "posixmodule.c"],
            "pwd": ["pwdmodule.c"],
            "pyexpat": ["expat/xmlparse.c", "expat/xmlrole.c", "expat/xmltok.c", "pyexpat.c", "-I$(srcdir)/Modules/expat", "-DHAVE_EXPAT_CONFIG_H", "-DUSE_PYEXPAT_CAPI", "-DXML_DEV_URANDOM"],
            "readline": ["readline.c", "-lreadline", "-ltermcap"],
            "resource": ["resource.c"],
            "select": ["selectmodule.c"],
            "spwd": ["spwdmodule.c"],
            "syslog": ["syslogmodule.c"],
            "termios": ["termios.c"],
            "time": ["-DPy_BUILD_CORE_BUILTIN", "-I$(srcdir)/Include/internal", "timemodule.c"],
            "unicodedata": ["unicodedata.c"],
            "zlib": ["zlibmodule.c", "-lz"],
        },

        core = [
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
        ],

        shared = [],

        static = [
            "_asyncio",
            "_bisect",
            "_blake2",
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
            "_scproxy",
            "_sha1",
            "_sha256",
            "_sha3",
            "_sha512",
            "_socket",
            "_sqlite3",
            "_ssl",
            "_statistics",
            "_struct",
            "_typing",
            "_uuid",
            "_zoneinfo",
            "array",
            "binascii",
            "cmath",
            "fcntl",
            "grp",
            "math",
            "mmap",
            "pyexpat",
            "readline",
            "select",
            "unicodedata",
            "zlib",
        ],

        disabled = [
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
            # "_decimal",
            "_dbm",
            "_tkinter",
            "_xxsubinterpreters",
            "audioop",
            "nis",
            "resource",
            "spwd",
            "syslog",
            "termios",
            "xxlimited",
            "xxlimited_35",
        ],
    ))

class PythonConfig_311:
    version: str = '3.11.7'
    base_cfg: Config = BASE_CONFIG

    def __init__(self):
        self.base_cfg = self.patch(self.base_cfg)

    def patch(self, base_cfg):
        return base_cfg

    def static_max(self):
        cfg = self.base_cfg.clone()
        cfg.write("static.local")
        return cfg

    def static_mid(self):
        cfg = self.base_cfg.clone()
        cfg.disable_static('_decimal')
        cfg.write("static.local")
        return cfg

    def __repr__(self):
        return f"<{self.__class__.__name__} '{self.version}'>"


class PythonConfig_312(PythonConfig_311):
    version = "3.12.2"
    base_cfg = BASE_CONFIG

    def patch(self, base_cfg):
        base_cfg.cfg['extensions'].update({
            '_md5':  ['md5module.c', '-I$(srcdir)/Modules/_hacl/include', '_hacl/Hacl_Hash_MD5.c', '-D_BSD_SOURCE', '-D_DEFAULT_SOURCE'], 
            '_sha1': ['sha1module.c', '-I$(srcdir)/Modules/_hacl/include', '_hacl/Hacl_Hash_SHA1.c', '-D_BSD_SOURCE', '-D_DEFAULT_SOURCE'], 
            '_sha2': ['sha2module.c', '-I$(srcdir)/Modules/_hacl/include', 'Modules/_hacl/libHacl_Hash_SHA2.a'], 
            '_sha3': ['sha3module.c', '-I$(srcdir)/Modules/_hacl/include', '_hacl/Hacl_Hash_SHA3.c', '-D_BSD_SOURCE', '-D_DEFAULT_SOURCE'],
        })
        del base_cfg.cfg['extensions']['_sha256']
        del base_cfg.cfg['extensions']['_sha512']
        base_cfg.cfg['static'].append('_sha2')
        base_cfg.cfg['static'].remove('_sha256')
        base_cfg.cfg['static'].remove('_sha512')
        return base_cfg



if __name__ == "__main__":
    cfg = PythonConfig_312()
    cfg.static_mid()

