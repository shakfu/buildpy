#![allow(clippy::vec_init_then_push)]

use std::collections::HashMap;
use std::env;

use serde::{Deserialize, Serialize};
use semver::Version;

use crate::config::macros::{hashmaps, vecs};
use crate::ops::log;

const PLATFORM: &str = env::consts::OS;

/// Model configuration for Python
/// 
/// # Examples
/// 
/// ```
/// use crate::config::model;
/// let config = model::Config::new("python", "3.12.11");
/// ```
#[derive(Serialize, Deserialize, Debug, Default)]
pub struct Config {
    pub name: String,
    pub version: String,
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
    pub headers: Vec<String>,
    pub exts: HashMap<String, Vec<String>>,
    pub core: Vec<String>,
    pub statik: Vec<String>,
    pub shared: Vec<String>,
    pub disabled: Vec<String>,

}

/// Create a new model configuration
impl Config {
    pub fn new(name: String, version: String) -> Self {
        let smver = Version::parse(&version).unwrap();
        Self {
            name: name.clone(),
            version: version.clone(),
            major: smver.major,
            minor: smver.minor,
            patch: smver.patch,
            headers: vecs![
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
            ],
            exts: hashmaps! {
                "name" => vecs!["name", "dest"],
                "_abc" => vecs!["_abc.c"],
                "_asyncio" => vecs!["_asynciomodule.c"],
                "_bisect"  => vecs!["_bisectmodule.c"],
                "_blake2"  => vecs![
                    "_blake2/blake2module.c",
                    "_blake2/blake2b_impl.c",
                    "_blake2/blake2s_impl.c"],
                "_bz2" => vecs![
                    "_bz2module.c",
                    "-I$(BZIP2)/include",
                    "-L$(BZIP2)/lib",
                    "$(BZIP2)/lib/libbz2.a"],
                "_codecs" => vecs!["_codecsmodule.c"],
                "_codecs_cn" => vecs!["cjkcodecs/_codecs_cn.c"],
                "_codecs_hk" => vecs!["cjkcodecs/_codecs_hk.c"],
                "_codecs_iso2022" => vecs!["cjkcodecs/_codecs_iso2022.c"],
                "_codecs_jp" => vecs!["cjkcodecs/_codecs_jp.c"],
                "_codecs_kr" => vecs!["cjkcodecs/_codecs_kr.c"],
                "_codecs_tw" => vecs!["cjkcodecs/_codecs_tw.c"],
                "_collections" => vecs!["_collectionsmodule.c"],
                "_contextvars" => vecs!["_contextvarsmodule.c"],
                "_csv" => vecs!["_csv.c"],
                "_ctypes" => vecs![
                    "_ctypes/_ctypes.c",
                    "_ctypes/callbacks.c",
                    "_ctypes/callproc.c",
                    "_ctypes/stgdict.c",
                    "_ctypes/cfield.c",
                    "-ldl",
                    "-lffi",
                    "-DHAVE_FFI_PREP_CIF_VAR",
                    "-DHAVE_FFI_PREP_CLOSURE_LOC",
                    "-DHAVE_FFI_CLOSURE_ALLOC"],
                "_curses"       => vecs!["-lncurses", "-lncursesw", "-ltermcap", "_cursesmodule.c"],
                "_curses_panel" => vecs!["-lpanel", "-lncurses", "_curses_panel.c"],
                "_datetime"     => vecs!["_datetimemodule.c"],
                "_dbm"          => vecs!["_dbmmodule.c", "-lgdbm_compat", "-DUSE_GDBM_COMPAT"],
                "_decimal"      => vecs!["_decimal/_decimal.c", "-DCONFIG_64=1"],
                "_elementtree"  => vecs!["_elementtree.c"],
                "_functools"    => vecs![
                    "-DPy_BUILD_CORE_BUILTIN",
                    "-I$(srcdir)/Include/internal",
                    "_functoolsmodule.c"],
                "_gdbm" => vecs!["_gdbmmodule.c", "-lgdbm"],
                "_hashlib" =>
                    vecs![
                    "_hashopenssl.c",
                    "-I$(OPENSSL)/include",
                    "-L$(OPENSSL)/lib",
                    "$(OPENSSL)/lib/libcrypto.a"],
                "_heapq" => vecs!["_heapqmodule.c"],
                "_io" => vecs![
                    "_io/_iomodule.c",
                    "_io/iobase.c",
                    "_io/fileio.c",
                    "_io/bytesio.c",
                    "_io/bufferedio.c",
                    "_io/textio.c",
                    "_io/stringio.c"],
                "_json" => vecs!["_json.c"],
                "_locale" => vecs!["-DPy_BUILD_CORE_BUILTIN", "_localemodule.c"],
                "_lsprof" => vecs!["_lsprof.c", "rotatingtree.c"],
                "_lzma" => vecs![
                    "_lzmamodule.c",
                    "-I$(LZMA)/include",
                    "-L$(LZMA)/lib",
                    "$(LZMA)/lib/liblzma.a"],
                "_md5" => vecs!["md5module.c"],
                "_multibytecodec" => vecs!["cjkcodecs/multibytecodec.c"],
                "_multiprocessing" => vecs![
                    "_multiprocessing/multiprocessing.c",
                    "_multiprocessing/semaphore.c"],
                "_opcode" => vecs!["_opcode.c"],
                "_operator" => vecs!["_operator.c"],
                "_pickle" => vecs!["_pickle.c"],
                "_posixshmem" => vecs!["_multiprocessing/posixshmem.c"],
                "_posixsubprocess" => vecs!["_posixsubprocess.c"],
                "_queue" => vecs!["_queuemodule.c"],
                "_random" => vecs!["_randommodule.c"],
                "_scproxy" => vecs!["_scproxy.c"],
                "_sha1" => vecs!["sha1module.c"],
                "_sha256" => vecs!["sha256module.c"],
                "_sha3" => vecs!["_sha3/sha3module.c"],
                "_sha512" => vecs!["sha512module.c"],
                "_signal" => vecs![
                    "-DPy_BUILD_CORE_BUILTIN",
                    "-I$(srcdir)/Include/internal",
                    "signalmodule.c"],
                "_socket" => vecs!["socketmodule.c"],
                "_sqlite3" => vecs![
                    "_sqlite/blob.c",
                    "_sqlite/connection.c",
                    "_sqlite/cursor.c",
                    "_sqlite/microprotocols.c",
                    "_sqlite/module.c",
                    "_sqlite/prepare_protocol.c",
                    "_sqlite/row.c",
                    "_sqlite/statement.c",
                    "_sqlite/util.c"],
                "_sre" => vecs!["_sre/sre.c", "-DPy_BUILD_CORE_BUILTIN"],
                "_ssl" => vecs![
                    "_ssl.c",
                    "-I$(OPENSSL)/include",
                    "-L$(OPENSSL)/lib",
                    "$(OPENSSL)/lib/libcrypto.a",
                    "$(OPENSSL)/lib/libssl.a"],
                "_stat" => vecs!["_stat.c"],
                "_statistics" => vecs!["_statisticsmodule.c"],
                "_struct" => vecs!["_struct.c"],
                "_symtable" => vecs!["symtablemodule.c"],
                "_thread" => vecs![
                        "-DPy_BUILD_CORE_BUILTIN",
                        "-I$(srcdir)/Include/internal",
                        "_threadmodule.c"],
                "_tracemalloc" => vecs!["_tracemalloc.c"],
                "_typing" => vecs!["_typingmodule.c"],
                "_uuid" => vecs!["_uuidmodule.c"],
                "_weakref" => vecs!["_weakref.c"],
                "_zoneinfo" => vecs!["_zoneinfo.c"],
                "array" => vecs!["arraymodule.c"],
                "atexit" => vecs!["atexitmodule.c"],
                "binascii" => vecs!["binascii.c"],
                "cmath" => vecs!["cmathmodule.c"],
                "errno" => vecs!["errnomodule.c"],
                "faulthandler" => vecs!["faulthandler.c"],
                "fcntl" => vecs!["fcntlmodule.c"],
                "grp" => vecs!["grpmodule.c"],
                "itertools" => vecs!["itertoolsmodule.c"],
                "math" => vecs!["mathmodule.c"],
                "mmap" => vecs!["mmapmodule.c"],
                "ossaudiodev" => vecs!["ossaudiodev.c"],
                "posix" => vecs![
                    "-DPy_BUILD_CORE_BUILTIN",
                    "-I$(srcdir)/Include/internal",
                    "posixmodule.c"],
                "pwd" => vecs!["pwdmodule.c"],
                "pyexpat" => vecs![
                    "expat/xmlparse.c",
                    "expat/xmlrole.c",
                    "expat/xmltok.c",
                    "pyexpat.c",
                    "-I$(srcdir)/Modules/expat",
                    "-DHAVE_EXPAT_CONFIG_H",
                    "-DUSE_PYEXPAT_CAPI",
                    "-DXML_DEV_URANDOM"],
                "readline" => vecs!["readline.c", "-lreadline", "-ltermcap"],
                "resource" => vecs!["resource.c"],
                "select" => vecs!["selectmodule.c"],
                "spwd" => vecs!["spwdmodule.c"],
                "syslog" => vecs!["syslogmodule.c"],
                "termios" => vecs!["termios.c"],
                "time" => vecs![
                    "-DPy_BUILD_CORE_BUILTIN",
                    "-I$(srcdir)/Include/internal",
                    "timemodule.c"],
                "unicodedata" => vecs!["unicodedata.c"],
                "zlib" => vecs!["zlibmodule.c", "-lz"],
            },
            core: vecs![
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
            ],
            statik: vecs![
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
                "zlib"
            ],
            shared: vecs![],
            disabled: vecs![
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
                "xxlimited_35"
            ],
        }
    }

    pub fn ver(&self) -> String {
        let split: Vec<String> = self.version.split('.').map(|s| s.to_string()).collect();
        format!("{}.{}", split[0], split[1])
    }

    fn sort_all(&mut self) {
        // self.core.sort();
        self.shared.sort();
        self.statik.sort();
        self.disabled.sort();
    }

    fn add_section(&self, header: &str, section: Vec<String>, lines: &mut Vec<String>) {
        if !section.is_empty() {
            lines.push(header.to_string());
            for elem in section {
                let mut lv = vec![elem.clone()];
                for l in &self.exts[&elem] {
                    lv.push(l.to_string());
                }
                lines.push(lv.join(" "));
            }
        }
    }

    pub fn write<P: AsRef<std::path::Path>>(&mut self, path: P) {
        self.sort_all();
        let mut lines = self.headers.clone();
        self.add_section("\n# core\n", self.core.clone(), &mut lines);
        self.add_section("\n*shared*\n", self.shared.clone(), &mut lines);
        self.add_section("\n*static*\n", self.statik.clone(), &mut lines);
        // disabled does not need lookups
        lines.push("\n*disabled*\n".to_string());
        lines.extend(self.disabled.clone());
        let _ = std::fs::write(path, lines.join("\n"));
    }

    pub fn static_to_shared(&mut self, names: Vec<String>) {
        for name in names {
            self.statik.retain(|x| x != &name);
            self.shared.push(name);
        }
    }

    pub fn shared_to_static(&mut self, names: Vec<String>) {
        for name in names {
            self.shared.retain(|x| x != &name);
            self.statik.push(name);
        }
    }

    pub fn static_to_disabled(&mut self, names: Vec<String>) {
        for name in names {
            self.statik.retain(|x| x != &name);
            self.disabled.push(name);
        }
    }

    pub fn disabled_to_static(&mut self, names: Vec<String>) {
        for name in names {
            self.disabled.retain(|x| x != &name);
            self.statik.push(name);
        }
    }

    pub fn shared_to_disabled(&mut self, names: Vec<String>) {
        for name in names {
            self.shared.retain(|x| x != &name);
            self.disabled.push(name);
        }
    }

    pub fn disabled_to_shared(&mut self, names: Vec<String>) {
        for name in names {
            self.disabled.retain(|x| x != &name);
            self.shared.push(name);
        }
    }

    pub fn configure(&mut self) {
        log::debug!(
            "config: plat: {PLATFORM} cfg: {} version: {}",
            self.name,
            self.version
        );

        if PLATFORM == "darwin" {
            log::debug!("config: common > darwin");
            self.disabled_to_static(vecs!["_scproxy"]);
        }
        if PLATFORM == "linux" {
            log::debug!("config: common > linux");

            self.disabled_to_static(vecs!["ossaudiodev"]);

            self.exts.insert(
                "_ssl".to_string(),
                vecs![
                    "_ssl.c",
                    "-I$(OPENSSL)/include",
                    "-L$(OPENSSL)/lib",
                    "-l:libssl.a -Wl,--exclude-libs,libssl.a",
                    "-l:libcrypto.a -Wl,--exclude-libs,libcrypto.a"
                ],
            );
            self.exts.insert(
                "_hashlib".to_string(),
                vecs![
                    "_hashopenssl.c",
                    "-I$(OPENSSL)/include",
                    "-L$(OPENSSL)/lib",
                    "-l:libcrypto.a -Wl,--exclude-libs,libcrypto.a"
                ],
            );
        }

        if self.minor >= 11 {
            if self.name == "static_max" {
                log::debug!("config: 3.11 -> static_max");
                if PLATFORM == "linux" {
                    self.static_to_disabled(vecs!["_decimal"]);
                }
            } else if self.name == "static_mid" {
                log::debug!("config: 3.11 -> static_mid");
                self.static_to_disabled(vecs!["_decimal"]);
            } else if self.name == "static_min" {
                log::debug!("config: 3.11 -> static_min");
                self.static_to_disabled(vecs![
                    "_bz2", "_decimal", "_csv", "_json", "_lzma", "_scproxy", "_sqlite3", "_ssl",
                    "pyexpat", "readline"
                ]);
            } else if self.name == "shared_max" {
                log::debug!("config: 3.11 -> shared_max");
                if PLATFORM == "linux" {
                    self.static_to_disabled(vecs!["_decimal"]);
                } else {
                    self.disabled_to_shared(vecs!["_ctypes"]);
                    self.static_to_shared(vecs!["_decimal", "_ssl", "_hashlib"]);
                }
            } else if self.name == "shared_mid" {
                log::debug!("config: 3.11 -> shared_mid");
                self.static_to_disabled(vecs!["_decimal", "_ssl", "_hashlib"]);
            }
        }
        
        if self.minor >= 12 {
            self.exts.insert(
                "_md5".to_string(),
                vecs![
                    "md5module.c",
                    "-I$(srcdir)/Modules/_hacl/include",
                    "_hacl/Hacl_Hash_MD5.c",
                    "-D_BSD_SOURCE",
                    "-D_DEFAULT_SOURCE"
                ],
            );

            self.exts.insert(
                "_sha1".to_string(),
                vecs![
                    "sha1module.c",
                    "-I$(srcdir)/Modules/_hacl/include",
                    "_hacl/Hacl_Hash_SHA1.c",
                    "-D_BSD_SOURCE",
                    "-D_DEFAULT_SOURCE"
                ],
            );

            self.exts.insert(
                "_sha2".to_string(),
                vecs![
                    "sha2module.c",
                    "-I$(srcdir)/Modules/_hacl/include",
                    "_hacl/Hacl_Hash_SHA2.c",
                    "-D_BSD_SOURCE",
                    "-D_DEFAULT_SOURCE",
                    "Modules/_hacl/libHacl_Hash_SHA2.a"
                ],
            );

            self.exts.insert(
                "_sha3".to_string(),
                vecs![
                    "sha3module.c",
                    "-I$(srcdir)/Modules/_hacl/include",
                    "_hacl/Hacl_Hash_SHA3.c",
                    "-D_BSD_SOURCE",
                    "-D_DEFAULT_SOURCE"
                ],
            );

            if self.exts.contains_key("_sha256") {
                self.exts.remove("_sha256");
            }
            if self.exts.contains_key("_sha512") {
                self.exts.remove("_sha512");
            }

            self.statik.push("_sha2".to_string());
            self.disabled.push("_xxinterpchannels".to_string());

            self.statik.retain(|x| *x != "_sha256");
            self.statik.retain(|x| *x != "_sha512");

            if self.name == "static_max" {
                log::debug!("config: 3.12 -> static_max");
                if PLATFORM == "linux" {
                    log::debug!("config: 3.12 > static_max > linux");
                    // self.static_to_disabled(vecs!["_decimal"]);
                }
            } else if self.name == "static_mid" {
                log::debug!("config: 3.12 -> static_mid");
                // self.static_to_disabled(vecs!["_decimal"]);
            } else if self.name == "static_min" {
                log::debug!("config: 3.12 > static_min");
                // self.static_to_disabled(vecs![
                //     "_bz2", "_decimal", "_csv", "_json", "_lzma", "_scproxy", "_sqlite3", "_ssl",
                //     "pyexpat", "readline"
                // ]);
            } else if self.name == "shared_max" {
                log::debug!("config: 3.12 -> shared_max");
                // self.disabled_to_shared(vecs!["_ctypes"]);
                // self.static_to_shared(vecs!["_decimal", "_ssl", "_hashlib"]);
            } else if self.name == "shared_mid" {
                log::debug!("config: 3.12 -> shared_max");
                // self.static_to_disabled(vecs!["_decimal", "_ssl", "_hashlib"]);
            }
        }

        if self.minor >= 13 {

            log::debug!("config: 3.13");

            self.exts.insert("_interpchannels".to_string(), vecs!["_interpchannelsmodule.c"]);
            self.exts.insert("_interpqueues".to_string(), vecs!["_interpqueuesmodule.c"]);
            self.exts.insert("_interpreters".to_string(), vecs!["_interpretersmodule.c"]);
            self.exts.insert("_sysconfig".to_string(), vecs!["_sysconfig.c"]);
            self.exts.insert("_testexternalinspection".to_string(), vecs!["_testexternalinspection.c"]);
           
            self.exts.remove("_crypt");
            self.exts.remove("ossaudiodev");
            self.exts.remove("spwd");

            self.statik.push("_interpchannels".to_string());
            self.statik.push("_interpqueues".to_string());
            self.statik.push("_interpreters".to_string());
            self.statik.push("_sysconfig".to_string());

            self.disabled.retain(|x| *x != "_crypt");
            self.disabled.retain(|x| *x != "_xxsubinterpreters");
            self.disabled.retain(|x| *x != "audioop");
            self.disabled.retain(|x| *x != "nis");
            self.disabled.retain(|x| *x != "ossaudiodev");
            self.disabled.retain(|x| *x != "spwd");

            // self.disabled.remove("_crypt");
            // self.disabled.remove("_xxsubinterpreters");
            // self.disabled.remove("audioop");
            // self.disabled.remove("nis");
            // self.disabled.remove("ossaudiodev");
            // self.disabled.remove("spwd");

            self.disabled.push("_testexternalinspection".to_string());
        }

    }
}
