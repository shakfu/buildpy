open Shell

type dependency = {
  name: string;
  version: string;
  download_url: string;
  repo_url: string;
  repo_branch: string option;
  static_libs: string list;
  project: Project.project;
  build_sys: Config.build_system;
}

let create_dependency name version download_url repo_url ?repo_branch static_libs project build_sys =
  { name; version; download_url; repo_url; repo_branch; static_libs; project; build_sys }

let prefix dep = Filename.concat dep.project.install dep.name

let src_dir dep = Filename.concat dep.project.src dep.name

let build_dir dep = Filename.concat (src_dir dep) "build"

let static_libs_exist dep =
  List.for_all (fun lib ->
    let lib_path = Filename.concat (Filename.concat (prefix dep) "lib") lib in
    Sys.file_exists lib_path
  ) dep.static_libs

let git_clone dep =
  let dest = src_dir dep in
  Shell.git_clone ?branch:dep.repo_branch dep.repo_url dest

let install_openssl () =
  let project = Project.create_project () in
  let ssl = create_dependency
    "openssl"
    "1.1.1w"
    "https://www.openssl.org/source/old/1.1.1/openssl-1.1.1w.tar.gz"
    "https://github.com/openssl/openssl.git"
    ~repo_branch:"OpenSSL_1_1_1w"
    ["libssl.a"; "libcrypto.a"]
    project
    Config.CONFIG
  in
  if not (static_libs_exist ssl) then (
    Project.setup ssl.project;
    let prefix_opt = "--prefix=" ^ (prefix ssl) in
    git_clone ssl;
    run_command ~cwd:(Some (src_dir ssl)) "./config" ["no-shared"; "no-tests"; prefix_opt];
    Shell.make ~cwd:(Some (src_dir ssl)) "install_sw" [];
    if not (static_libs_exist ssl) then
      failwith "Could not build openssl"
  )

let install_bzip2 () =
  let project = Project.create_project () in
  let bz2 = create_dependency
    "bzip2"
    "1.0.8"
    "https://sourceware.org/pub/bzip2/bzip2-1.0.8.tar.gz"
    "https://github.com/libarchive/bzip2.git"
    ~repo_branch:"bzip2-1.0.8"
    ["libbz2.a"]
    project
    Config.MAKE
  in
  if not (static_libs_exist bz2) then (
    Project.setup bz2.project;
    let prefix_opt = "PREFIX=" ^ (prefix bz2) in
    git_clone bz2;
    Shell.make ~cwd:(Some (src_dir bz2)) "install" [prefix_opt; "CFLAGS='-fPIC'"];
    if not (static_libs_exist bz2) then
      failwith "Could not build bzip2"
  )

let install_xz () =
  let project = Project.create_project () in
  let xz = create_dependency
    "xz"
    "5.6.3"
    "https://github.com/tukaani-project/xz/releases/download/v5.6.3/xz-5.6.3.tar.gz"
    "https://github.com/python/cpython-source-deps.git"
    ~repo_branch:"xz"
    ["liblzma.a"]
    project
    Config.CMAKE
  in
  if not (static_libs_exist xz) then (
    Project.setup xz.project;
    git_clone xz;
    let configure_script = Filename.concat (src_dir xz) "configure" in
    let install_sh = Filename.concat (Filename.concat (src_dir xz) "build-aux") "install-sh" in
    Unix.chmod configure_script 0o755;
    Unix.chmod install_sh 0o755;
    let prefix_opt = "--prefix=" ^ (prefix xz) in
    let configure_args = [
      "./configure";
      "--disable-dependency-tracking";
      "--disable-xzdec";
      "--disable-lzmadec";
      "--disable-nls";
      "--enable-small";
      "--disable-shared";
      prefix_opt;
    ] in
    run_command ~cwd:(Some (src_dir xz)) (List.hd configure_args) (List.tl configure_args);
    Shell.make ~cwd:(Some (src_dir xz)) "install" [];
    if not (static_libs_exist xz) then
      failwith "Could not build lzma"
  )

let install_dependencies () =
  log_info "Installing dependencies...";
  install_openssl ();
  install_bzip2 ();
  install_xz ()