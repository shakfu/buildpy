/*
Copyright © 2024 NAME HERE <EMAIL ADDRESS>
*/
package config

import (
	"fmt"
	"os"
	"strings"
	"text/template"
)

const Template string = `
# -*- makefile -*-
$ version: {{.Version}}
{{- range .Headers }}
{{ . -}}
{{- end }}

# core

{{- range $_, $key := .Core}}
{{ $key }} {{ join (index $.Exts $key) " " -}}
{{- end }}

*static*

{{- range $key, $value := .Static}}
{{ $key }} {{ join (index $.Exts $key) " " -}}
{{- end }}

*shared*

{{- range $key, $value := .Shared}}
{{ $key }} {{ join (index $.Exts $key) " " -}}
{{- end }}

*disabled*

{{- range $key, $value := .Disabled}}
{{ $key -}}
{{- end }}

# end
`

type Config struct {
	Version  string
	Headers  []string
	Exts     map[string][]string
	Core     []string
	Static   map[string]bool
	Shared   map[string]bool
	Disabled map[string]bool
}

func (c *Config) StaticToShared(names ...string) {
	for _, name := range names {
		delete(c.Static, name)
		c.Shared[name] = true
	}
}

func (c *Config) SharedToStatic(names ...string) {
	for _, name := range names {
		delete(c.Shared, name)
		c.Static[name] = true
	}
}

func (c *Config) StaticToDisabled(names ...string) {
	for _, name := range names {
		delete(c.Static, name)
		c.Disabled[name] = true
	}
}

func (c *Config) SharedToDisabled(names ...string) {
	for _, name := range names {
		delete(c.Shared, name)
		c.Disabled[name] = true
	}
}

func (c *Config) CommentShared(names ...string) {
	for _, name := range names {
		c.Shared[name] = false
	}
}

func (c *Config) UncommentShared(names ...string) {
	for _, name := range names {
		c.Shared[name] = true
	}
}

func (c *Config) CommentStatic(names ...string) {
	for _, name := range names {
		c.Static[name] = false
	}
}

func (c *Config) UnommentStatic(names ...string) {
	for _, name := range names {
		c.Static[name] = true
	}
}

func (c *Config) CommentDisabled(names ...string) {
	for _, name := range names {
		c.Disabled[name] = false
	}
}

func (c *Config) UnommentDisabled(names ...string) {
	for _, name := range names {
		c.Disabled[name] = true
	}
}

var base_cfg = Config{
	Version: "3.11",
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
		"_abc":     []string{"_abc.c"},
		"_asyncio": []string{"_asynciomodule.c"},
		"_bisect":  []string{"_bisectmodule.c"},
		"_blake2": []string{
			"_blake2/blake2module.c",
			"_blake2/blake2b_impl.c",
			"_blake2/blake2s_impl.c",
		},

		"_bz2": []string{
			"_bz2module.c",
			"-I$(BZIP2)/include",
			"-L$(BZIP2)/lib",
			"$(BZIP2)/lib/libbz2.a",
		},
		"_codecs":         []string{"_codecsmodule.c"},
		"_codecs_cn":      []string{"cjkcodecs/_codecs_cn.c"},
		"_codecs_hk":      []string{"cjkcodecs/_codecs_hk.c"},
		"_codecs_iso2022": []string{"cjkcodecs/_codecs_iso2022.c"},
		"_codecs_jp":      []string{"cjkcodecs/_codecs_jp.c"},
		"_codecs_kr":      []string{"cjkcodecs/_codecs_kr.c"},
		"_codecs_tw":      []string{"cjkcodecs/_codecs_tw.c"},
		"_collections":    []string{"_collectionsmodule.c"},
		"_contextvars":    []string{"_contextvarsmodule.c"},
		"_csv":            []string{"_csv.c"},
		"_ctypes": []string{
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
		"_curses":       []string{"-lncurses", "-lncursesw", "-ltermcap", "_cursesmodule.c"},
		"_curses_panel": []string{"-lpanel", "-lncurses", "_curses_panel.c"},
		"_datetime":     []string{"_datetimemodule.c"},
		"_dbm":          []string{"_dbmmodule.c", "-lgdbm_compat", "-DUSE_GDBM_COMPAT"},
		"_decimal":      []string{"_decimal/_decimal.c", "-DCONFIG_64=1"},
		"_elementtree":  []string{"_elementtree.c"},
		"_functools": []string{
			"-DPy_BUILD_CORE_BUILTIN",
			"-I$(srcdir)/Include/internal",
			"_functoolsmodule.c",
		},
		"_gdbm": []string{"_gdbmmodule.c", "-lgdbm"},
		"_hashlib": []string{
			"_hashopenssl.c",
			"-I$(OPENSSL)/include",
			"-L$(OPENSSL)/lib",
			"$(OPENSSL)/lib/libcrypto.a",
		},
		"_heapq": []string{"_heapqmodule.c"},
		"_io": []string{
			"_io/_iomodule.c",
			"_io/iobase.c",
			"_io/fileio.c",
			"_io/bytesio.c",
			"_io/bufferedio.c",
			"_io/textio.c",
			"_io/stringio.c",
		},
		"_json":   []string{"_json.c"},
		"_locale": []string{"-DPy_BUILD_CORE_BUILTIN", "_localemodule.c"},
		"_lsprof": []string{"_lsprof.c", "rotatingtree.c"},
		"_lzma": []string{
			"_lzmamodule.c",
			"-I$(LZMA)/include",
			"-L$(LZMA)/lib",
			"$(LZMA)/lib/liblzma.a",
		},
		"_md5":            []string{"md5module.c"},
		"_multibytecodec": []string{"cjkcodecs/multibytecodec.c"},
		"_multiprocessing": []string{
			"_multiprocessing/multiprocessing.c",
			"_multiprocessing/semaphore.c",
		},
		"_opcode":          []string{"_opcode.c"},
		"_operator":        []string{"_operator.c"},
		"_pickle":          []string{"_pickle.c"},
		"_posixshmem":      []string{"_multiprocessing/posixshmem.c"},
		"_posixsubprocess": []string{"_posixsubprocess.c"},
		"_queue":           []string{"_queuemodule.c"},
		"_random":          []string{"_randommodule.c"},
		"_scproxy":         []string{"_scproxy.c"},
		"_sha1":            []string{"sha1module.c"},
		"_sha256":          []string{"sha256module.c"},
		"_sha3":            []string{"_sha3/sha3module.c"},
		"_sha512":          []string{"sha512module.c"},
		"_signal": []string{
			"-DPy_BUILD_CORE_BUILTIN",
			"-I$(srcdir)/Include/internal",
			"signalmodule.c",
		},
		"_socket": []string{"socketmodule.c"},
		"_sqlite3": []string{
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
		"_sre": []string{"_sre/sre.c", "-DPy_BUILD_CORE_BUILTIN"},
		"_ssl": []string{
			"_ssl.c",
			"-I$(OPENSSL)/include",
			"-L$(OPENSSL)/lib",
			"$(OPENSSL)/lib/libcrypto.a",
			"$(OPENSSL)/lib/libssl.a",
		},
		"_stat":       []string{"_stat.c"},
		"_statistics": []string{"_statisticsmodule.c"},
		"_struct":     []string{"_struct.c"},
		"_symtable":   []string{"symtablemodule.c"},
		"_thread": []string{
			"-DPy_BUILD_CORE_BUILTIN",
			"-I$(srcdir)/Include/internal",
			"_threadmodule.c",
		},
		"_tracemalloc": []string{"_tracemalloc.c"},
		"_typing":      []string{"_typingmodule.c"},
		"_uuid":        []string{"_uuidmodule.c"},
		"_weakref":     []string{"_weakref.c"},
		"_zoneinfo":    []string{"_zoneinfo.c"},
		"array":        []string{"arraymodule.c"},
		"atexit":       []string{"atexitmodule.c"},
		"binascii":     []string{"binascii.c"},
		"cmath":        []string{"cmathmodule.c"},
		"errno":        []string{"errnomodule.c"},
		"faulthandler": []string{"faulthandler.c"},
		"fcntl":        []string{"fcntlmodule.c"},
		"grp":          []string{"grpmodule.c"},
		"itertools":    []string{"itertoolsmodule.c"},
		"math":         []string{"mathmodule.c"},
		"mmap":         []string{"mmapmodule.c"},
		"ossaudiodev":  []string{"ossaudiodev.c"},
		"posix": []string{
			"-DPy_BUILD_CORE_BUILTIN",
			"-I$(srcdir)/Include/internal",
			"posixmodule.c",
		},
		"pwd": []string{"pwdmodule.c"},
		"pyexpat": []string{
			"expat/xmlparse.c",
			"expat/xmlrole.c",
			"expat/xmltok.c",
			"pyexpat.c",
			"-I$(srcdir)/Modules/expat",
			"-DHAVE_EXPAT_CONFIG_H",
			"-DUSE_PYEXPAT_CAPI",
			"-DXML_DEV_URANDOM",
		},
		"readline": []string{"readline.c", "-lreadline", "-ltermcap"},
		"resource": []string{"resource.c"},
		"select":   []string{"selectmodule.c"},
		"spwd":     []string{"spwdmodule.c"},
		"syslog":   []string{"syslogmodule.c"},
		"termios":  []string{"termios.c"},
		"time": []string{
			"-DPy_BUILD_CORE_BUILTIN",
			"-I$(srcdir)/Include/internal",
			"timemodule.c",
		},
		"unicodedata": []string{"unicodedata.c"},
		"zlib":        []string{"zlibmodule.c", "-lz"},
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

	Static: map[string]bool{
		"_asyncio":         true,
		"_bisect":          true,
		"_blake2":          true,
		"_bz2":             true,
		"_contextvars":     true,
		"_csv":             true,
		"_datetime":        true,
		"_decimal":         true,
		"_elementtree":     true,
		"_hashlib":         true,
		"_heapq":           true,
		"_json":            true,
		"_lsprof":          true,
		"_lzma":            true,
		"_md5":             true,
		"_multibytecodec":  true,
		"_multiprocessing": true,
		"_opcode":          true,
		"_pickle":          true,
		"_posixshmem":      true,
		"_posixsubprocess": true,
		"_queue":           true,
		"_random":          true,
		"_sha1":            true,
		"_sha256":          true,
		"_sha3":            true,
		"_sha512":          true,
		"_socket":          true,
		"_sqlite3":         true,
		"_ssl":             true,
		"_statistics":      true,
		"_struct":          true,
		"_typing":          true,
		"_uuid":            true,
		"_zoneinfo":        true,
		"array":            true,
		"binascii":         true,
		"cmath":            true,
		"fcntl":            true,
		"grp":              true,
		"math":             true,
		"mmap":             true,
		"pyexpat":          true,
		"readline":         true,
		"select":           true,
		"unicodedata":      true,
		"zlib":             true,
	},

	Shared: map[string]bool{},

	Disabled: map[string]bool{
		"_codecs_cn":         true,
		"_codecs_hk":         true,
		"_codecs_iso2022":    true,
		"_codecs_jp":         true,
		"_codecs_kr":         true,
		"_codecs_tw":         true,
		"_crypt":             true,
		"_ctypes":            true,
		"_curses":            true,
		"_curses_panel":      true,
		"_dbm":               true,
		"_scproxy":           true,
		"_tkinter":           true,
		"_xxsubinterpreters": true,
		"audioop":            true,
		"nis":                true,
		"ossaudiodev":        true,
		"resource":           true,
		"spwd":               true,
		"syslog":             true,
		"termios":            true,
		"xxlimited":          true,
		"xxlimited_35":       true,
	},
}

func Demo() {

	cfg1 := &base_cfg
	cfg1.StaticToShared("cmath", "zlib")
	cfg1.CommentStatic("select")
	// fmt.Println(cfg1)
	fmt.Println("=>")

	funcMap := template.FuncMap{
		// The name "title" is what the function will be called in the template text.
		"title": strings.Title,
		"join":  strings.Join,
	}

	tmpl, err := template.New("test").Funcs(funcMap).Parse(Template)
	if err != nil {
		panic(err)
	}
	err = tmpl.Execute(os.Stdout, cfg1)
	if err != nil {
		panic(err)
	}

}
