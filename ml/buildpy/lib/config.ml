
type build_system = CMAKE | CONFIG | MAKE

type config = {
  name: string;
  version: string;
  headers: string list;
  exts: (string * string list) list;
  core: string list;
  static: string list;
  shared: string list;
  disabled: string list;
}

let platform =
  match Sys.os_type with
  | "Unix" ->
    (match Unix.getenv "OSTYPE" with
     | exception Not_found -> "linux"
     | "darwin" -> "darwin"
     | _ -> "linux")
  | _ -> "linux"

let default_headers = [
  "DESTLIB=$(LIBDEST)";
  "MACHDESTLIB=$(BINLIBDEST)";
  "DESTPATH=";
  "SITEPATH=";
  "TESTPATH=";
  "COREPYTHONPATH=$(DESTPATH)$(SITEPATH)$(TESTPATH)";
  "PYTHONPATH=$(COREPYTHONPATH)";
  "OPENSSL=$(srcdir)/../../install/openssl";
  "BZIP2=$(srcdir)/../../install/bzip2";
  "LZMA=$(srcdir)/../../install/xz";
]

let default_exts = [
  ("_abc", ["_abc.c"]);
  ("_asyncio", ["_asynciomodule.c"]);
  ("_bisect", ["_bisectmodule.c"]);
  ("_blake2", [
    "_blake2/blake2module.c";
    "_blake2/blake2b_impl.c";
    "_blake2/blake2s_impl.c";
  ]);
  ("_bz2", [
    "_bz2module.c";
    "-I$(BZIP2)/include";
    "-L$(BZIP2)/lib";
    "$(BZIP2)/lib/libbz2.a";
  ]);
  ("_codecs", ["_codecsmodule.c"]);
  ("_codecs_cn", ["cjkcodecs/_codecs_cn.c"]);
  ("_codecs_hk", ["cjkcodecs/_codecs_hk.c"]);
  ("_codecs_iso2022", ["cjkcodecs/_codecs_iso2022.c"]);
  ("_codecs_jp", ["cjkcodecs/_codecs_jp.c"]);
  ("_codecs_kr", ["cjkcodecs/_codecs_kr.c"]);
  ("_codecs_tw", ["cjkcodecs/_codecs_tw.c"]);
  ("_collections", ["_collectionsmodule.c"]);
  ("_contextvars", ["_contextvarsmodule.c"]);
  ("_csv", ["_csv.c"]);
  ("_ctypes", [
    "_ctypes/_ctypes.c";
    "_ctypes/callbacks.c";
    "_ctypes/callproc.c";
    "_ctypes/stgdict.c";
    "_ctypes/cfield.c";
    "-ldl";
    "-lffi";
    "-DHAVE_FFI_PREP_CIF_VAR";
    "-DHAVE_FFI_PREP_CLOSURE_LOC";
    "-DHAVE_FFI_CLOSURE_ALLOC";
  ]);
  ("_curses", ["-lncurses"; "-lncursesw"; "-ltermcap"; "_cursesmodule.c"]);
  ("_curses_panel", ["-lpanel"; "-lncurses"; "_curses_panel.c"]);
  ("_datetime", ["_datetimemodule.c"]);
  ("_dbm", ["_dbmmodule.c"; "-lgdbm_compat"; "-DUSE_GDBM_COMPAT"]);
  ("_decimal", ["_decimal/_decimal.c"; "-DCONFIG_64=1"]);
  ("_elementtree", ["_elementtree.c"]);
  ("_functools", [
    "-DPy_BUILD_CORE_BUILTIN";
    "-I$(srcdir)/Include/internal";
    "_functoolsmodule.c";
  ]);
  ("_gdbm", ["_gdbmmodule.c"; "-lgdbm"]);
  ("_hashlib", [
    "_hashopenssl.c";
    "-I$(OPENSSL)/include";
    "-L$(OPENSSL)/lib";
    "$(OPENSSL)/lib/libcrypto.a";
  ]);
  ("_heapq", ["_heapqmodule.c"]);
  ("_io", [
    "_io/_iomodule.c";
    "_io/iobase.c";
    "_io/fileio.c";
    "_io/bytesio.c";
    "_io/bufferedio.c";
    "_io/textio.c";
    "_io/stringio.c";
  ]);
  ("_json", ["_json.c"]);
  ("_locale", ["-DPy_BUILD_CORE_BUILTIN"; "_localemodule.c"]);
  ("_lsprof", ["_lsprof.c"; "rotatingtree.c"]);
  ("_lzma", [
    "_lzmamodule.c";
    "-I$(LZMA)/include";
    "-L$(LZMA)/lib";
    "$(LZMA)/lib/liblzma.a";
  ]);
  ("_md5", ["md5module.c"]);
  ("_multibytecodec", ["cjkcodecs/multibytecodec.c"]);
  ("_multiprocessing", [
    "_multiprocessing/multiprocessing.c";
    "_multiprocessing/semaphore.c";
  ]);
  ("_opcode", ["_opcode.c"]);
  ("_operator", ["_operator.c"]);
  ("_pickle", ["_pickle.c"]);
  ("_posixshmem", ["_multiprocessing/posixshmem.c"]);
  ("_posixsubprocess", ["_posixsubprocess.c"]);
  ("_queue", ["_queuemodule.c"]);
  ("_random", ["_randommodule.c"]);
  ("_scproxy", ["_scproxy.c"]);
  ("_sha1", ["sha1module.c"]);
  ("_sha256", ["sha256module.c"]);
  ("_sha3", ["_sha3/sha3module.c"]);
  ("_sha512", ["sha512module.c"]);
  ("_signal", [
    "-DPy_BUILD_CORE_BUILTIN";
    "-I$(srcdir)/Include/internal";
    "signalmodule.c";
  ]);
  ("_socket", ["socketmodule.c"]);
  ("_sqlite3", [
    "_sqlite/blob.c";
    "_sqlite/connection.c";
    "_sqlite/cursor.c";
    "_sqlite/microprotocols.c";
    "_sqlite/module.c";
    "_sqlite/prepare_protocol.c";
    "_sqlite/row.c";
    "_sqlite/statement.c";
    "_sqlite/util.c";
  ]);
  ("_sre", ["_sre/sre.c"; "-DPy_BUILD_CORE_BUILTIN"]);
  ("_ssl", [
    "_ssl.c";
    "-I$(OPENSSL)/include";
    "-L$(OPENSSL)/lib";
    "$(OPENSSL)/lib/libcrypto.a";
    "$(OPENSSL)/lib/libssl.a";
  ]);
  ("_stat", ["_stat.c"]);
  ("_statistics", ["_statisticsmodule.c"]);
  ("_struct", ["_struct.c"]);
  ("_symtable", ["symtablemodule.c"]);
  ("_thread", [
    "-DPy_BUILD_CORE_BUILTIN";
    "-I$(srcdir)/Include/internal";
    "_threadmodule.c";
  ]);
  ("_tracemalloc", ["_tracemalloc.c"]);
  ("_typing", ["_typingmodule.c"]);
  ("_uuid", ["_uuidmodule.c"]);
  ("_weakref", ["_weakref.c"]);
  ("_zoneinfo", ["_zoneinfo.c"]);
  ("array", ["arraymodule.c"]);
  ("atexit", ["atexitmodule.c"]);
  ("binascii", ["binascii.c"]);
  ("cmath", ["cmathmodule.c"]);
  ("errno", ["errnomodule.c"]);
  ("faulthandler", ["faulthandler.c"]);
  ("fcntl", ["fcntlmodule.c"]);
  ("grp", ["grpmodule.c"]);
  ("itertools", ["itertoolsmodule.c"]);
  ("math", ["mathmodule.c"]);
  ("mmap", ["mmapmodule.c"]);
  ("ossaudiodev", ["ossaudiodev.c"]);
  ("posix", [
    "-DPy_BUILD_CORE_BUILTIN";
    "-I$(srcdir)/Include/internal";
    "posixmodule.c";
  ]);
  ("pwd", ["pwdmodule.c"]);
  ("pyexpat", [
    "expat/xmlparse.c";
    "expat/xmlrole.c";
    "expat/xmltok.c";
    "pyexpat.c";
    "-I$(srcdir)/Modules/expat";
    "-DHAVE_EXPAT_CONFIG_H";
    "-DUSE_PYEXPAT_CAPI";
    "-DXML_DEV_URANDOM";
  ]);
  ("readline", ["readline.c"; "-lreadline"; "-ltermcap"]);
  ("resource", ["resource.c"]);
  ("select", ["selectmodule.c"]);
  ("spwd", ["spwdmodule.c"]);
  ("syslog", ["syslogmodule.c"]);
  ("termios", ["termios.c"]);
  ("time", [
    "-DPy_BUILD_CORE_BUILTIN";
    "-I$(srcdir)/Include/internal";
    "timemodule.c";
  ]);
  ("unicodedata", ["unicodedata.c"]);
  ("zlib", ["zlibmodule.c"; "-lz"]);
]

let default_core = [
  "_abc";
  "_codecs";
  "_collections";
  "_functools";
  "_io";
  "_locale";
  "_operator";
  "_signal";
  "_sre";
  "_stat";
  "_symtable";
  "_thread";
  "_tracemalloc";
  "_weakref";
  "atexit";
  "errno";
  "faulthandler";
  "itertools";
  "posix";
  "pwd";
  "time";
]

let default_static = [
  "_asyncio";
  "_bisect";
  "_blake2";
  "_bz2";
  "_contextvars";
  "_csv";
  "_datetime";
  "_decimal";
  "_elementtree";
  "_hashlib";
  "_heapq";
  "_json";
  "_lsprof";
  "_lzma";
  "_md5";
  "_multibytecodec";
  "_multiprocessing";
  "_opcode";
  "_pickle";
  "_posixshmem";
  "_posixsubprocess";
  "_queue";
  "_random";
  "_sha1";
  "_sha256";
  "_sha3";
  "_sha512";
  "_socket";
  "_sqlite3";
  "_ssl";
  "_statistics";
  "_struct";
  "_typing";
  "_uuid";
  "_zoneinfo";
  "array";
  "binascii";
  "cmath";
  "fcntl";
  "grp";
  "math";
  "mmap";
  "pyexpat";
  "readline";
  "select";
  "unicodedata";
  "zlib";
]

let default_disabled = [
  "_codecs_cn";
  "_codecs_hk";
  "_codecs_iso2022";
  "_codecs_jp";
  "_codecs_kr";
  "_codecs_tw";
  "_crypt";
  "_ctypes";
  "_curses";
  "_curses_panel";
  "_dbm";
  "_scproxy";
  "_tkinter";
  "_xxsubinterpreters";
  "audioop";
  "nis";
  "ossaudiodev";
  "resource";
  "spwd";
  "syslog";
  "termios";
  "xxlimited";
  "xxlimited_35";
]

let create_config name version =
  {
    name;
    version;
    headers = default_headers;
    exts = default_exts;
    core = default_core;
    static = default_static;
    shared = [];
    disabled = default_disabled;
  }

let parse_version version_str =
  let parts = String.split_on_char '.' version_str in
  match parts with
  | [major; minor; _] -> (int_of_string major, int_of_string minor)
  | [major; minor] -> (int_of_string major, int_of_string minor)
  | _ -> failwith ("Invalid version format: " ^ version_str)

let move_static_to_shared config modules =
  let new_static = List.filter (fun m -> not (List.mem m modules)) config.static in
  let new_shared = List.fold_left (fun acc m ->
    if List.mem m config.static then m :: acc else acc
  ) config.shared modules in
  { config with static = new_static; shared = new_shared }

let move_static_to_disabled config modules =
  let new_static = List.filter (fun m -> not (List.mem m modules)) config.static in
  let new_disabled = List.fold_left (fun acc m ->
    if List.mem m config.static then m :: acc else acc
  ) config.disabled modules in
  { config with static = new_static; disabled = new_disabled }

let move_disabled_to_static config modules =
  let new_disabled = List.filter (fun m -> not (List.mem m modules)) config.disabled in
  let new_static = List.fold_left (fun acc m ->
    if List.mem m config.disabled then m :: acc else acc
  ) config.static modules in
  { config with disabled = new_disabled; static = new_static }

let move_disabled_to_shared config modules =
  let new_disabled = List.filter (fun m -> not (List.mem m modules)) config.disabled in
  let new_shared = List.fold_left (fun acc m ->
    if List.mem m config.disabled then m :: acc else acc
  ) config.shared modules in
  { config with disabled = new_disabled; shared = new_shared }

let configure_for_platform_and_version config =
  let (_, minor) = parse_version config.version in
  let config =
    match platform with
    | "darwin" ->
      move_disabled_to_static config ["_scproxy"]
    | "linux" ->
      move_disabled_to_static config ["ossaudiodev"]
    | _ -> config
  in

  let config =
    if minor >= 11 then
      match config.name with
      | "static_max" ->
        if platform = "linux" then
          move_static_to_disabled config ["_decimal"]
        else config
      | "static_mid" ->
        move_static_to_disabled config ["_decimal"]
      | "static_min" ->
        move_static_to_disabled config ["_bz2"; "_decimal"; "_csv"; "_json";
                                        "_lzma"; "_scproxy"; "_sqlite3"; "_ssl";
                                        "pyexpat"; "readline"]
      | "shared_max" ->
        if platform = "linux" then
          move_static_to_disabled config ["_decimal"]
        else
          let config = move_disabled_to_shared config ["_ctypes"] in
          move_static_to_shared config ["_decimal"; "_ssl"; "_hashlib"]
      | "shared_mid" ->
        move_static_to_disabled config ["_decimal"; "_ssl"; "_hashlib"]
      | _ -> config
    else config
  in

  let config =
    if minor >= 12 then
      let config = move_static_to_disabled config ["_sha256"; "_sha512"] in
      let new_static = "_sha2" :: config.static in
      let new_disabled = "_xxinterpchannels" :: config.disabled in
      { config with static = new_static; disabled = new_disabled }
    else config
  in

  let config =
    if minor >= 13 then
      let new_static = ["_interpchannels"; "_interpqueues"; "_interpreters"; "_sysconfig"] @ config.static in
      let new_disabled = ["_testexternalinspection"] @ config.disabled in
      let filtered_disabled = List.filter (fun m ->
        not (List.mem m ["_crypt"; "_xxsubinterpreters"; "audioop"; "nis"; "ossaudiodev"; "spwd"])
      ) new_disabled in
      { config with static = new_static; disabled = filtered_disabled }
    else config
  in

  config

let generate_setup_local config =
  let buffer = Buffer.create 1024 in

  List.iter (fun header ->
    Buffer.add_string buffer (header ^ "\n")
  ) config.headers;

  Buffer.add_string buffer "\n";

  List.iter (fun module_name ->
    match List.assoc_opt module_name config.exts with
    | Some files ->
      Buffer.add_string buffer (module_name ^ " " ^ (String.concat " " files) ^ "\n")
    | None -> ()
  ) config.static;

  Buffer.add_string buffer "\n*shared*\n\n";

  List.iter (fun module_name ->
    match List.assoc_opt module_name config.exts with
    | Some files ->
      Buffer.add_string buffer (module_name ^ " " ^ (String.concat " " files) ^ "\n")
    | None -> ()
  ) config.shared;

  Buffer.add_string buffer "\n*disabled*\n\n";

  List.iter (fun module_name ->
    Buffer.add_string buffer (module_name ^ "\n")
  ) config.disabled;

  Buffer.contents buffer

let write_setup_local config path =
  let content = generate_setup_local config in
  let oc = open_out path in
  output_string oc content;
  close_out oc