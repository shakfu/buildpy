open Shell

type python_builder = {
  name: string;
  version: string;
  config_name: string;
  download_url: string;
  repo_url: string;
  config_options: string list;
  packages: string list;
  remove_patterns: string list;
  project: Project.project;
  optimize: bool;
  use_git: bool;
  jobs: int;
}

let platform =
  match Sys.os_type with
  | "Unix" ->
    (match Unix.getenv "OSTYPE" with
     | exception Not_found -> "linux"
     | "darwin" -> "darwin"
     | _ -> "linux")
  | _ -> "linux"

let arch =
  match Sys.command "uname -m 2>/dev/null" with
  | 0 ->
    let ic = Unix.open_process_in "uname -m" in
    let arch = String.trim (input_line ic) in
    let _ = Unix.close_process_in ic in
    (match arch with
     | "x86_64" -> "x86_64"
     | "arm64" | "aarch64" -> "arm64"
     | _ -> "x86_64")
  | _ -> "x86_64"

let create_python_builder version config_name =
  let remove_patterns = [
    "*.exe";
    "*config-3*";
    "*tcl*";
    "*tdbc*";
    "*tk*";
    "__phello__";
    "__pycache__";
    "_codecs_*.so";
    "_ctypes_test*";
    "_test*";
    "_tk*";
    "_xx*.so";
    "distutils";
    "idlelib";
    "lib2to3";
    "LICENSE.txt";
    "pkgconfig";
    "pydoc_data";
    "site-packages";
    "test";
    "Tk*";
    "turtle*";
    "venv";
    "xx*.so";
  ] in
  {
    name = "Python";
    version;
    config_name;
    download_url = "https://www.python.org/ftp/python/%s/Python-%s.tar.xz";
    repo_url = "https://github.com/python/cpython.git";
    config_options = ["--disable-test-modules"];
    packages = [];
    remove_patterns;
    project = Project.create_project ();
    optimize = false;
    use_git = false;
    jobs = 1;
  }

let replace_first str pattern replacement =
  let pattern_len = String.length pattern in
  let str_len = String.length str in
  let rec find_pos pos =
    if pos + pattern_len > str_len then
      str
    else if String.sub str pos pattern_len = pattern then
      let before = String.sub str 0 pos in
      let after = String.sub str (pos + pattern_len) (str_len - pos - pattern_len) in
      before ^ replacement ^ after
    else
      find_pos (pos + 1)
  in
  find_pos 0

let url builder =
  let template = builder.download_url in
  let replaced_once = replace_first template "%s" builder.version in
  replace_first replaced_once "%s" builder.version

let prefix builder = Filename.concat builder.project.install "python"

let src_dir builder = Filename.concat builder.project.src "python"

let build_dir builder = Filename.concat (src_dir builder) "build"

let contains_substring str sub =
  let str_len = String.length str in
  let sub_len = String.length sub in
  let rec check pos =
    if pos + sub_len > str_len then false
    else if String.sub str pos sub_len = sub then true
    else check (pos + 1)
  in
  check 0

let get_build_name builder =
  let size_type = match builder.config_name with
    | s when contains_substring s "min" -> "min"
    | s when contains_substring s "mid" -> "mid"
    | s when contains_substring s "max" -> "max"
    | _ -> "max"
  in
  let build_type = if contains_substring builder.config_name "shared" then "shared" else "static" in
  Printf.sprintf "py-%s-%s-%s-%s-%s" build_type size_type builder.version platform arch

let download_python builder =
  let download_file = Filename.concat builder.project.downloads ("Python-" ^ builder.version ^ ".tar.xz") in
  if not (Sys.file_exists download_file) then
    Shell.download_file (url builder) download_file;
  download_file

let extract_python builder download_file =
  let dest_dir = src_dir builder in
  if not (Sys.file_exists dest_dir) then
    Shell.extract_archive download_file dest_dir

let git_clone_python builder =
  let dest_dir = src_dir builder in
  let branch = "v" ^ builder.version in
  Shell.git_clone ~branch builder.repo_url dest_dir

let setup_local_config builder =
  let config = Config.create_config builder.config_name builder.version in
  let configured_config = Config.configure_for_platform_and_version config in
  let setup_local_path = Filename.concat (src_dir builder) "Modules/Setup.local" in
  Config.write_setup_local configured_config setup_local_path;
  log_info ("Generated Setup.local at: " ^ setup_local_path)

let configure_python builder =
  let configure_script = Filename.concat (src_dir builder) "configure" in
  let prefix_opt = "--prefix=" ^ (prefix builder) in
  let all_opts = prefix_opt :: builder.config_options in
  Shell.run_command ~cwd:(Some (src_dir builder)) configure_script all_opts

let build_python builder =
  Shell.make ~jobs:builder.jobs ~cwd:(Some (src_dir builder)) "all" []

let install_python builder =
  Shell.make ~cwd:(Some (src_dir builder)) "install" []

let clean_installation builder =
  let install_dir = prefix builder in
  Shell.log_info "Cleaning installation...";
  Shell.remove_files_matching builder.remove_patterns install_dir

let zip_stdlib builder =
  let lib_dir = Filename.concat (prefix builder) ("lib/python" ^ (String.sub builder.version 0 3)) in
  let zip_file = Filename.concat lib_dir "python-stdlib.zip" in
  if Sys.file_exists lib_dir then (
    Shell.zip_directory lib_dir zip_file;
    Shell.log_info ("Created stdlib zip: " ^ zip_file)
  )

let install_packages builder =
  if builder.packages <> [] then (
    Shell.log_info "Installing additional packages...";
    let pip_cmd = Filename.concat (Filename.concat (prefix builder) "bin") "pip3" in
    List.iter (fun pkg ->
      Shell.run_command pip_cmd ["install"; pkg]
    ) builder.packages
  )

let pre_process builder =
  Shell.log_info "Pre-processing...";
  Project.setup builder.project;
  Dependency.install_dependencies ()

let setup builder =
  Shell.log_info "Setting up Python source...";
  if builder.use_git then
    git_clone_python builder
  else (
    let download_file = download_python builder in
    extract_python builder download_file
  );
  setup_local_config builder

let configure builder =
  Shell.log_info "Configuring Python build...";
  configure_python builder

let build builder =
  Shell.log_info "Building Python...";
  build_python builder

let install builder =
  Shell.log_info "Installing Python...";
  install_python builder

let post_process builder =
  Shell.log_info "Post-processing installation...";
  clean_installation builder;
  zip_stdlib builder;
  install_packages builder;
  let build_name = get_build_name builder in
  Shell.log_info ("Build completed: " ^ build_name)

let process builder =
  try
    pre_process builder;
    setup builder;
    configure builder;
    build builder;
    install builder;
    post_process builder;
    Shell.log_info "Python build process completed successfully!"
  with
  | Shell.Command_failed (cmd, code) ->
    Shell.log_error (Printf.sprintf "Command failed: %s (exit code: %d)" cmd code);
    exit code
  | exn ->
    Shell.log_error ("Build failed with exception: " ^ (Printexc.to_string exn));
    exit 1