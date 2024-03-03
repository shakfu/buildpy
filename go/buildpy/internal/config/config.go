package config

import (
	"github.com/charmbracelet/log"
	"gopkg.in/yaml.v3"
	"os"
	"sort"
	"strings"
	"text/template"
	"runtime"
)

var PLATFORM = runtime.GOOS

type Config struct {
	Name     string
	Version  string
	Headers  []string
	Exts     map[string][]string
	Core     []string
	Static   []string
	Shared   []string
	Disabled []string
}

func NewConfig(name string, version string) *Config {
	return &Config{
		Name:    name,
		Version: version,
		Headers: []string{
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
		},
		Exts: map[string][]string{
			"_abc":     {"_abc.c"},
			"_asyncio": {"_asynciomodule.c"},
			"_bisect":  {"_bisectmodule.c"},
			"_blake2": {
				"_blake2/blake2module.c",
				"_blake2/blake2b_impl.c",
				"_blake2/blake2s_impl.c",
			},

			"_bz2": {
				"_bz2module.c",
				"-I$(BZIP2)/include",
				"-L$(BZIP2)/lib",
				"$(BZIP2)/lib/libbz2.a",
			},
			"_codecs":         {"_codecsmodule.c"},
			"_codecs_cn":      {"cjkcodecs/_codecs_cn.c"},
			"_codecs_hk":      {"cjkcodecs/_codecs_hk.c"},
			"_codecs_iso2022": {"cjkcodecs/_codecs_iso2022.c"},
			"_codecs_jp":      {"cjkcodecs/_codecs_jp.c"},
			"_codecs_kr":      {"cjkcodecs/_codecs_kr.c"},
			"_codecs_tw":      {"cjkcodecs/_codecs_tw.c"},
			"_collections":    {"_collectionsmodule.c"},
			"_contextvars":    {"_contextvarsmodule.c"},
			"_csv":            {"_csv.c"},
			"_ctypes": {
				"_ctypes/_ctypes.c",
				"_ctypes/callbacks.c",
				"_ctypes/callproc.c",
				"_ctypes/stgdict.c",
				"_ctypes/cfield.c",
				"-ldl",
				"-lffi",
				"-DHAVE_FFI_PREP_CIF_VAR",
				"-DHAVE_FFI_PREP_CLOSURE_LOC",
				"-DHAVE_FFI_CLOSURE_ALLOC",
			},
			"_curses":       {"-lncurses", "-lncursesw", "-ltermcap", "_cursesmodule.c"},
			"_curses_panel": {"-lpanel", "-lncurses", "_curses_panel.c"},
			"_datetime":     {"_datetimemodule.c"},
			"_dbm":          {"_dbmmodule.c", "-lgdbm_compat", "-DUSE_GDBM_COMPAT"},
			"_decimal":      {"_decimal/_decimal.c", "-DCONFIG_64=1"},
			"_elementtree":  {"_elementtree.c"},
			"_functools": {
				"-DPy_BUILD_CORE_BUILTIN",
				"-I$(srcdir)/Include/internal",
				"_functoolsmodule.c",
			},
			"_gdbm": {"_gdbmmodule.c", "-lgdbm"},
			"_hashlib": {
				"_hashopenssl.c",
				"-I$(OPENSSL)/include",
				"-L$(OPENSSL)/lib",
				"$(OPENSSL)/lib/libcrypto.a",
			},
			"_heapq": {"_heapqmodule.c"},
			"_io": {
				"_io/_iomodule.c",
				"_io/iobase.c",
				"_io/fileio.c",
				"_io/bytesio.c",
				"_io/bufferedio.c",
				"_io/textio.c",
				"_io/stringio.c",
			},
			"_json":   {"_json.c"},
			"_locale": {"-DPy_BUILD_CORE_BUILTIN", "_localemodule.c"},
			"_lsprof": {"_lsprof.c", "rotatingtree.c"},
			"_lzma": {
				"_lzmamodule.c",
				"-I$(LZMA)/include",
				"-L$(LZMA)/lib",
				"$(LZMA)/lib/liblzma.a",
			},
			"_md5":            {"md5module.c"},
			"_multibytecodec": {"cjkcodecs/multibytecodec.c"},
			"_multiprocessing": {
				"_multiprocessing/multiprocessing.c",
				"_multiprocessing/semaphore.c",
			},
			"_opcode":          {"_opcode.c"},
			"_operator":        {"_operator.c"},
			"_pickle":          {"_pickle.c"},
			"_posixshmem":      {"_multiprocessing/posixshmem.c"},
			"_posixsubprocess": {"_posixsubprocess.c"},
			"_queue":           {"_queuemodule.c"},
			"_random":          {"_randommodule.c"},
			"_scproxy":         {"_scproxy.c"},
			"_sha1":            {"sha1module.c"},
			"_sha256":          {"sha256module.c"},
			"_sha3":            {"_sha3/sha3module.c"},
			"_sha512":          {"sha512module.c"},
			"_signal": {
				"-DPy_BUILD_CORE_BUILTIN",
				"-I$(srcdir)/Include/internal",
				"signalmodule.c",
			},
			"_socket": {"socketmodule.c"},
			"_sqlite3": {
				"_sqlite/blob.c",
				"_sqlite/connection.c",
				"_sqlite/cursor.c",
				"_sqlite/microprotocols.c",
				"_sqlite/module.c",
				"_sqlite/prepare_protocol.c",
				"_sqlite/row.c",
				"_sqlite/statement.c",
				"_sqlite/util.c",
			},
			"_sre": {"_sre/sre.c", "-DPy_BUILD_CORE_BUILTIN"},
			"_ssl": {
				"_ssl.c",
				"-I$(OPENSSL)/include",
				"-L$(OPENSSL)/lib",
				"$(OPENSSL)/lib/libcrypto.a",
				"$(OPENSSL)/lib/libssl.a",
			},
			"_stat":       {"_stat.c"},
			"_statistics": {"_statisticsmodule.c"},
			"_struct":     {"_struct.c"},
			"_symtable":   {"symtablemodule.c"},
			"_thread": {
				"-DPy_BUILD_CORE_BUILTIN",
				"-I$(srcdir)/Include/internal",
				"_threadmodule.c",
			},
			"_tracemalloc": {"_tracemalloc.c"},
			"_typing":      {"_typingmodule.c"},
			"_uuid":        {"_uuidmodule.c"},
			"_weakref":     {"_weakref.c"},
			"_zoneinfo":    {"_zoneinfo.c"},
			"array":        {"arraymodule.c"},
			"atexit":       {"atexitmodule.c"},
			"binascii":     {"binascii.c"},
			"cmath":        {"cmathmodule.c"},
			"errno":        {"errnomodule.c"},
			"faulthandler": {"faulthandler.c"},
			"fcntl":        {"fcntlmodule.c"},
			"grp":          {"grpmodule.c"},
			"itertools":    {"itertoolsmodule.c"},
			"math":         {"mathmodule.c"},
			"mmap":         {"mmapmodule.c"},
			"ossaudiodev":  {"ossaudiodev.c"},
			"posix": {
				"-DPy_BUILD_CORE_BUILTIN",
				"-I$(srcdir)/Include/internal",
				"posixmodule.c",
			},
			"pwd": {"pwdmodule.c"},
			"pyexpat": {
				"expat/xmlparse.c",
				"expat/xmlrole.c",
				"expat/xmltok.c",
				"pyexpat.c",
				"-I$(srcdir)/Modules/expat",
				"-DHAVE_EXPAT_CONFIG_H",
				"-DUSE_PYEXPAT_CAPI",
				"-DXML_DEV_URANDOM",
			},
			"readline": {"readline.c", "-lreadline", "-ltermcap"},
			"resource": {"resource.c"},
			"select":   {"selectmodule.c"},
			"spwd":     {"spwdmodule.c"},
			"syslog":   {"syslogmodule.c"},
			"termios":  {"termios.c"},
			"time": {
				"-DPy_BUILD_CORE_BUILTIN",
				"-I$(srcdir)/Include/internal",
				"timemodule.c",
			},
			"unicodedata": {"unicodedata.c"},
			"zlib":        {"zlibmodule.c", "-lz"},
		},

		Core: []string{
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
		},

		Static: []string{
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
		},

		Shared: []string{},

		Disabled: []string{
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
			"_tkinter",
			"_xxsubinterpreters",
			"audioop",
			"nis",
			"ossaudiodev",
			"resource",
			"spwd",
			"syslog",
			"termios",
			"xxlimited",
			"xxlimited_35",
		},
	}
}

func (c *Config) Ver() string {
	return strings.Join(strings.Split(c.Version, ".")[:2], ".")
}

func (c *Config) MoveNames(src []string, dst []string, names ...string) {
	RemoveNames(src, names...)
	AddNames(dst, names...)
}

func (c *Config) StaticToShared(names ...string) {
	c.Static = RemoveNames(c.Static, names...)
	c.Shared = AddNames(c.Shared, names...)
}

func (c *Config) SharedToStatic(names ...string) {
	c.Shared = RemoveNames(c.Shared, names...)
	c.Static = AddNames(c.Static, names...)
}

func (c *Config) StaticToDisabled(names ...string) {
	c.Static = RemoveNames(c.Static, names...)
	c.Disabled = AddNames(c.Disabled, names...)
}

func (c *Config) SharedToDisabled(names ...string) {
	c.Static = RemoveNames(c.Static, names...)
	c.Shared = AddNames(c.Shared, names...)
}

func (c *Config) DisabledToStatic(names ...string) {
	c.Disabled = RemoveNames(c.Disabled, names...)
	c.Static = AddNames(c.Static, names...)
}

func (c *Config) DisabledToShared(names ...string) {
	c.Shared = RemoveNames(c.Shared, names...)
	c.Static = AddNames(c.Static, names...)
}

func (c *Config) Sort() {
	sort.Strings(c.Static)
	sort.Strings(c.Shared)
	sort.Strings(c.Disabled)
}

func (c *Config) Write(path string) {
	c.Sort()
	funcMap := template.FuncMap{
		"join": strings.Join,
	}

	tmpl, err := template.New("test").Funcs(funcMap).Parse(Template)
	if err != nil {
		panic(err)
	}
	createFileUsingTemplate(tmpl, path, c)
}

func (c *Config) WriteYaml(path string) {
	c.Sort()
	d, err := yaml.Marshal(&c)
	if err != nil {
		log.Fatalf("error: %v", err)
	}
	log.Printf("--- t dump:\n%s\n\n", string(d))

	f, err := os.Create(path)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	size, err := f.WriteString(string(d))
	if err != nil {
		log.Fatal(err)
	}
	log.Info("wrote yaml", "file", path, "size", size)

	// f.Sync()
}

func (c *Config) WriteSetupLocal(path string) {

    if PLATFORM == "darwin" {
        c.DisabledToStatic("_scproxy")
    }
    if PLATFORM == "linux" {
        c.DisabledToStatic("ossaudiodev")
    }

	if c.Ver() == "3.11" {

		if c.Name == "static_max" {
        	// fall through
 
		} else if c.Name == "static_mid" {

			c.StaticToDisabled("_decimal")

			if PLATFORM == "linux" {
	            c.Exts["_ssl"] = []string{
	                "_ssl.c",
	                "-I$(OPENSSL)/include",
	                "-L$(OPENSSL)/lib",
	                "-l:libssl.a -Wl,--exclude-libs,libssl.a",
	                "-l:libcrypto.a -Wl,--exclude-libs,libcrypto.a",
	            }
	            c.Exts["_hashlib"] = []string{
	                "_hashopenssl.c",
	                "-I$(OPENSSL)/include",
	                "-L$(OPENSSL)/lib",
	                "-l:libcrypto.a -Wl,--exclude-libs,libcrypto.a",
	            }
			}

		} else if c.Name == "static_min" {

			c.StaticToDisabled("_bz2", "_decimal", "_csv", "_json", "_lzma", "_scproxy", "_sqlite3", "_ssl", "pyexpat", "readline")

		} else if c.Name == "shared_max" {

			c.DisabledToShared("_ctypes")
			c.StaticToShared("_decimal", "_ssl", "_hashlib")

		} else if c.Name == "shared_mid" {
			c.StaticToDisabled("_decimal", "_ssl", "_hashlib")

		}

	} else if c.Ver() == "3.12" {

        c.Exts["_md5"] = []string{
            "md5module.c",
            "-I$(srcdir)/Modules/_hacl/include",
            "_hacl/Hacl_Hash_MD5.c",
            "-D_BSD_SOURCE",
            "-D_DEFAULT_SOURCE",
        }

        c.Exts["_sha1"] = []string{
	        "sha1module.c",
	        "-I$(srcdir)/Modules/_hacl/include",
	        "_hacl/Hacl_Hash_SHA1.c",
	        "-D_BSD_SOURCE",
	        "-D_DEFAULT_SOURCE",
        }

        c.Exts["_sha2"] = []string{
	        "sha2module.c",
	        "-I$(srcdir)/Modules/_hacl/include",
	        "_hacl/Hacl_Hash_SHA2.c",
	        "-D_BSD_SOURCE",
	        "-D_DEFAULT_SOURCE",
	        "Modules/_hacl/libHacl_Hash_SHA2.a",
        }

        c.Exts["_sha3"] = []string{
            "sha3module.c",
            "-I$(srcdir)/Modules/_hacl/include",
            "_hacl/Hacl_Hash_SHA3.c",
            "-D_BSD_SOURCE",
            "-D_DEFAULT_SOURCE",
        }

        delete(c.Exts, "_sha256")
        delete(c.Exts, "_sha512")

        c.Static = append(c.Static, "_sha2")
        c.Disabled = append(c.Static, "_xxinterpchannels")

        c.Static = RemoveNames(c.Static, "_sha256", "_sha512")

        if c.Name == "static_max" {
        	// fall through
        } else if c.Name == "static_mid" {

			c.StaticToDisabled("_decimal")

			if PLATFORM == "linux" {
	            c.Exts["_ssl"] = []string{
	                "_ssl.c",
	                "-I$(OPENSSL)/include",
	                "-L$(OPENSSL)/lib",
	                "-l:libssl.a -Wl,--exclude-libs,libssl.a",
	                "-l:libcrypto.a -Wl,--exclude-libs,libcrypto.a",
	            }
	            c.Exts["_hashlib"] = []string{
	                "_hashopenssl.c",
	                "-I$(OPENSSL)/include",
	                "-L$(OPENSSL)/lib",
	                "-l:libcrypto.a -Wl,--exclude-libs,libcrypto.a",
	            }
			}

        } else if c.Name == "static_min" {

			c.StaticToDisabled("_bz2", "_decimal", "_csv", "_json", "_lzma", "_scproxy", "_sqlite3", "_ssl", "pyexpat", "readline")

		} else if c.Name == "static_min" {

			c.DisabledToShared("_ctypes")
			c.StaticToShared("_decimal", "_ssl", "_hashlib")
		} else if c.Name == "static_min" {
			c.StaticToDisabled("_decimal", "_ssl", "_hashlib")
		}

	}

	c.Write(path)
}