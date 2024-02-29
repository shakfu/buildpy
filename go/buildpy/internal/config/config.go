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
