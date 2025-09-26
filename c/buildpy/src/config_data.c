#include "config.h"
#include "buildpy.h"

// Extension definitions initialization
void config_initialize_extensions(Config* config) {
    if (!config) return;

    // Define all Python extensions with their build files
    // Using C11 compound literals for cleaner initialization

    config_add_extension(config, "_abc", (const char*[]){"_abc.c"}, 1);
    config_add_extension(config, "_asyncio", (const char*[]){"_asynciomodule.c"}, 1);
    config_add_extension(config, "_bisect", (const char*[]){"_bisectmodule.c"}, 1);

    config_add_extension(config, "_blake2", (const char*[]){
        "_blake2/blake2module.c",
        "_blake2/blake2b_impl.c",
        "_blake2/blake2s_impl.c"
    }, 3);

    config_add_extension(config, "_bz2", (const char*[]){
        "_bz2module.c",
        "-I$(BZIP2)/include",
        "-L$(BZIP2)/lib",
        "$(BZIP2)/lib/libbz2.a"
    }, 4);

    config_add_extension(config, "_codecs", (const char*[]){"_codecsmodule.c"}, 1);
    config_add_extension(config, "_codecs_cn", (const char*[]){"cjkcodecs/_codecs_cn.c"}, 1);
    config_add_extension(config, "_codecs_hk", (const char*[]){"cjkcodecs/_codecs_hk.c"}, 1);
    config_add_extension(config, "_codecs_iso2022", (const char*[]){"cjkcodecs/_codecs_iso2022.c"}, 1);
    config_add_extension(config, "_codecs_jp", (const char*[]){"cjkcodecs/_codecs_jp.c"}, 1);
    config_add_extension(config, "_codecs_kr", (const char*[]){"cjkcodecs/_codecs_kr.c"}, 1);
    config_add_extension(config, "_codecs_tw", (const char*[]){"cjkcodecs/_codecs_tw.c"}, 1);
    config_add_extension(config, "_collections", (const char*[]){"_collectionsmodule.c"}, 1);
    config_add_extension(config, "_contextvars", (const char*[]){"_contextvarsmodule.c"}, 1);
    config_add_extension(config, "_csv", (const char*[]){"_csv.c"}, 1);

    config_add_extension(config, "_ctypes", (const char*[]){
        "_ctypes/_ctypes.c",
        "_ctypes/callbacks.c",
        "_ctypes/callproc.c",
        "_ctypes/stgdict.c",
        "_ctypes/cfield.c",
        "-ldl", "-lffi",
        "-DHAVE_FFI_PREP_CIF_VAR",
        "-DHAVE_FFI_PREP_CLOSURE_LOC",
        "-DHAVE_FFI_CLOSURE_ALLOC"
    }, 10);

    config_add_extension(config, "_curses", (const char*[]){
        "-lncurses", "-lncursesw", "-ltermcap", "_cursesmodule.c"
    }, 4);

    config_add_extension(config, "_curses_panel", (const char*[]){
        "-lpanel", "-lncurses", "_curses_panel.c"
    }, 3);

    config_add_extension(config, "_datetime", (const char*[]){"_datetimemodule.c"}, 1);
    config_add_extension(config, "_dbm", (const char*[]){"_dbmmodule.c", "-lgdbm_compat", "-DUSE_GDBM_COMPAT"}, 3);
    config_add_extension(config, "_decimal", (const char*[]){"_decimal/_decimal.c", "-DCONFIG_64=1"}, 2);
    config_add_extension(config, "_elementtree", (const char*[]){"_elementtree.c"}, 1);

    config_add_extension(config, "_functools", (const char*[]){
        "-DPy_BUILD_CORE_BUILTIN",
        "-I$(srcdir)/Include/internal",
        "_functoolsmodule.c"
    }, 3);

    config_add_extension(config, "_gdbm", (const char*[]){"_gdbmmodule.c", "-lgdbm"}, 2);

    config_add_extension(config, "_hashlib", (const char*[]){
        "_hashopenssl.c",
        "-I$(OPENSSL)/include",
        "-L$(OPENSSL)/lib",
        "$(OPENSSL)/lib/libcrypto.a"
    }, 4);

    config_add_extension(config, "_heapq", (const char*[]){"_heapqmodule.c"}, 1);

    config_add_extension(config, "_io", (const char*[]){
        "_io/_iomodule.c", "_io/iobase.c", "_io/fileio.c",
        "_io/bytesio.c", "_io/bufferedio.c", "_io/textio.c",
        "_io/stringio.c"
    }, 7);

    config_add_extension(config, "_json", (const char*[]){"_json.c"}, 1);
    config_add_extension(config, "_locale", (const char*[]){"-DPy_BUILD_CORE_BUILTIN", "_localemodule.c"}, 2);
    config_add_extension(config, "_lsprof", (const char*[]){"_lsprof.c", "rotatingtree.c"}, 2);

    config_add_extension(config, "_lzma", (const char*[]){
        "_lzmamodule.c",
        "-I$(LZMA)/include",
        "-L$(LZMA)/lib",
        "$(LZMA)/lib/liblzma.a"
    }, 4);

    config_add_extension(config, "_md5", (const char*[]){"md5module.c"}, 1);
    config_add_extension(config, "_multibytecodec", (const char*[]){"cjkcodecs/multibytecodec.c"}, 1);

    config_add_extension(config, "_multiprocessing", (const char*[]){
        "_multiprocessing/multiprocessing.c",
        "_multiprocessing/semaphore.c"
    }, 2);

    config_add_extension(config, "_opcode", (const char*[]){"_opcode.c"}, 1);
    config_add_extension(config, "_operator", (const char*[]){"_operator.c"}, 1);
    config_add_extension(config, "_pickle", (const char*[]){"_pickle.c"}, 1);
    config_add_extension(config, "_posixshmem", (const char*[]){"_multiprocessing/posixshmem.c"}, 1);
    config_add_extension(config, "_posixsubprocess", (const char*[]){"_posixsubprocess.c"}, 1);
    config_add_extension(config, "_queue", (const char*[]){"_queuemodule.c"}, 1);
    config_add_extension(config, "_random", (const char*[]){"_randommodule.c"}, 1);
    config_add_extension(config, "_scproxy", (const char*[]){"_scproxy.c"}, 1);
    config_add_extension(config, "_sha1", (const char*[]){"sha1module.c"}, 1);
    config_add_extension(config, "_sha256", (const char*[]){"sha256module.c"}, 1);
    config_add_extension(config, "_sha3", (const char*[]){"_sha3/sha3module.c"}, 1);
    config_add_extension(config, "_sha512", (const char*[]){"sha512module.c"}, 1);

    config_add_extension(config, "_signal", (const char*[]){
        "-DPy_BUILD_CORE_BUILTIN",
        "-I$(srcdir)/Include/internal",
        "signalmodule.c"
    }, 3);

    config_add_extension(config, "_socket", (const char*[]){"socketmodule.c"}, 1);

    config_add_extension(config, "_sqlite3", (const char*[]){
        "_sqlite/blob.c", "_sqlite/connection.c", "_sqlite/cursor.c",
        "_sqlite/microprotocols.c", "_sqlite/module.c", "_sqlite/prepare_protocol.c",
        "_sqlite/row.c", "_sqlite/statement.c", "_sqlite/util.c"
    }, 9);

    config_add_extension(config, "_sre", (const char*[]){"_sre/sre.c", "-DPy_BUILD_CORE_BUILTIN"}, 2);

    config_add_extension(config, "_ssl", (const char*[]){
        "_ssl.c",
        "-I$(OPENSSL)/include",
        "-L$(OPENSSL)/lib",
        "$(OPENSSL)/lib/libcrypto.a",
        "$(OPENSSL)/lib/libssl.a"
    }, 5);

    config_add_extension(config, "_stat", (const char*[]){"_stat.c"}, 1);
    config_add_extension(config, "_statistics", (const char*[]){"_statisticsmodule.c"}, 1);
    config_add_extension(config, "_struct", (const char*[]){"_struct.c"}, 1);
    config_add_extension(config, "_symtable", (const char*[]){"symtablemodule.c"}, 1);

    config_add_extension(config, "_thread", (const char*[]){
        "-DPy_BUILD_CORE_BUILTIN",
        "-I$(srcdir)/Include/internal",
        "_threadmodule.c"
    }, 3);

    config_add_extension(config, "_tracemalloc", (const char*[]){"_tracemalloc.c"}, 1);
    config_add_extension(config, "_typing", (const char*[]){"_typingmodule.c"}, 1);
    config_add_extension(config, "_uuid", (const char*[]){"_uuidmodule.c"}, 1);
    config_add_extension(config, "_weakref", (const char*[]){"_weakref.c"}, 1);
    config_add_extension(config, "_zoneinfo", (const char*[]){"_zoneinfo.c"}, 1);
    config_add_extension(config, "array", (const char*[]){"arraymodule.c"}, 1);
    config_add_extension(config, "atexit", (const char*[]){"atexitmodule.c"}, 1);
    config_add_extension(config, "binascii", (const char*[]){"binascii.c"}, 1);
    config_add_extension(config, "cmath", (const char*[]){"cmathmodule.c"}, 1);
    config_add_extension(config, "errno", (const char*[]){"errnomodule.c"}, 1);
    config_add_extension(config, "faulthandler", (const char*[]){"faulthandler.c"}, 1);
    config_add_extension(config, "fcntl", (const char*[]){"fcntlmodule.c"}, 1);
    config_add_extension(config, "grp", (const char*[]){"grpmodule.c"}, 1);
    config_add_extension(config, "itertools", (const char*[]){"itertoolsmodule.c"}, 1);
    config_add_extension(config, "math", (const char*[]){"mathmodule.c"}, 1);
    config_add_extension(config, "mmap", (const char*[]){"mmapmodule.c"}, 1);
    config_add_extension(config, "ossaudiodev", (const char*[]){"ossaudiodev.c"}, 1);

    config_add_extension(config, "posix", (const char*[]){
        "-DPy_BUILD_CORE_BUILTIN",
        "-I$(srcdir)/Include/internal",
        "posixmodule.c"
    }, 3);

    config_add_extension(config, "pwd", (const char*[]){"pwdmodule.c"}, 1);

    config_add_extension(config, "pyexpat", (const char*[]){
        "expat/xmlparse.c", "expat/xmlrole.c", "expat/xmltok.c", "pyexpat.c",
        "-I$(srcdir)/Modules/expat", "-DHAVE_EXPAT_CONFIG_H",
        "-DUSE_PYEXPAT_CAPI", "-DXML_DEV_URANDOM"
    }, 8);

    config_add_extension(config, "readline", (const char*[]){"readline.c", "-lreadline", "-ltermcap"}, 3);
    config_add_extension(config, "resource", (const char*[]){"resource.c"}, 1);
    config_add_extension(config, "select", (const char*[]){"selectmodule.c"}, 1);
    config_add_extension(config, "spwd", (const char*[]){"spwdmodule.c"}, 1);
    config_add_extension(config, "syslog", (const char*[]){"syslogmodule.c"}, 1);
    config_add_extension(config, "termios", (const char*[]){"termios.c"}, 1);

    config_add_extension(config, "time", (const char*[]){
        "-DPy_BUILD_CORE_BUILTIN",
        "-I$(srcdir)/Include/internal",
        "timemodule.c"
    }, 3);

    config_add_extension(config, "unicodedata", (const char*[]){"unicodedata.c"}, 1);
    config_add_extension(config, "zlib", (const char*[]){"zlibmodule.c", "-lz"}, 2);
}

void config_initialize_static_modules(Config* config) {
    if (!config) return;

    // Default static modules
    const char* static_modules[] = {
        "_asyncio", "_bisect", "_blake2", "_bz2", "_contextvars", "_csv",
        "_datetime", "_decimal", "_elementtree", "_hashlib", "_heapq", "_json",
        "_lsprof", "_lzma", "_md5", "_multibytecodec", "_multiprocessing",
        "_opcode", "_pickle", "_posixshmem", "_posixsubprocess", "_queue",
        "_random", "_sha1", "_sha256", "_sha3", "_sha512", "_socket", "_sqlite3",
        "_ssl", "_statistics", "_struct", "_typing", "_uuid", "_zoneinfo",
        "array", "binascii", "cmath", "fcntl", "grp", "math", "mmap",
        "pyexpat", "readline", "select", "unicodedata", "zlib"
    };

    int count = sizeof(static_modules) / sizeof(static_modules[0]);
    for (int i = 0; i < count; i++) {
        config_add_static_module(config, static_modules[i]);
    }
}

void config_initialize_disabled_modules(Config* config) {
    if (!config) return;

    // Default disabled modules
    const char* disabled_modules[] = {
        "_codecs_cn", "_codecs_hk", "_codecs_iso2022", "_codecs_jp",
        "_codecs_kr", "_codecs_tw", "_crypt", "_ctypes", "_curses",
        "_curses_panel", "_dbm", "_scproxy", "_tkinter", "_xxsubinterpreters",
        "audioop", "nis", "ossaudiodev", "resource", "spwd", "syslog",
        "termios", "xxlimited", "xxlimited_35"
    };

    int count = sizeof(disabled_modules) / sizeof(disabled_modules[0]);
    for (int i = 0; i < count; i++) {
        config_add_disabled_module(config, disabled_modules[i]);
    }
}