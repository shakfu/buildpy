open Printf
open Unix

exception Command_failed of string * int

let log_info msg = printf "[INFO] %s\n%!" msg
let log_error msg = printf "[ERROR] %s\n%!" msg

let run_command ?(cwd=None) ?(env=None) cmd args =
  let full_cmd = String.concat " " (cmd :: args) in
  log_info ("Running: " ^ full_cmd);

  let original_cwd = match cwd with
    | Some dir ->
      let current = getcwd () in
      chdir dir;
      Some current
    | None -> None
  in

  let cmd_line = full_cmd in
  let exit_code = match env with
    | Some env_vars ->
      let env_str = String.concat " " (List.map (fun (k, v) -> k ^ "=" ^ v) env_vars) in
      Sys.command (env_str ^ " " ^ cmd_line)
    | None -> Sys.command cmd_line
  in

  (match original_cwd with
   | Some dir -> chdir dir
   | None -> ());

  if exit_code <> 0 then
    raise (Command_failed (full_cmd, exit_code))

let makedirs dirs =
  List.iter (fun dir ->
    if not (Sys.file_exists dir) then (
      log_info ("Creating directory: " ^ dir);
      let rec make_path path =
        let parent = Filename.dirname path in
        if parent <> path && not (Sys.file_exists parent) then
          make_path parent;
        if not (Sys.file_exists path) then
          mkdir path 0o755
      in
      make_path dir
    )
  ) dirs

let download_file url dest_file =
  log_info ("Downloading: " ^ url ^ " -> " ^ dest_file);
  let cmd = "curl" in
  let args = ["-L"; "-o"; dest_file; url] in
  run_command cmd args

let git_clone ?branch repo_url dest_dir =
  log_info ("Cloning: " ^ repo_url ^ " -> " ^ dest_dir);
  if Sys.file_exists dest_dir then (
    log_info ("Directory exists, pulling updates: " ^ dest_dir);
    run_command ~cwd:(Some dest_dir) "git" ["pull"]
  ) else (
    let cmd = "git" in
    let args = match branch with
      | Some b -> ["clone"; "--branch"; b; "--depth"; "1"; repo_url; dest_dir]
      | None -> ["clone"; "--depth"; "1"; repo_url; dest_dir]
    in
    run_command cmd args
  )

let extract_archive archive_path dest_dir =
  log_info ("Extracting: " ^ archive_path ^ " -> " ^ dest_dir);
  makedirs [dest_dir];
  let cmd = "tar" in
  let args = ["-xf"; archive_path; "-C"; dest_dir; "--strip-components=1"] in
  run_command cmd args

let make ?(jobs=1) ?cwd target args =
  let cmd = "make" in
  let job_args = ["-j"; string_of_int jobs] in
  let all_args = job_args @ [target] @ args in
  run_command ?cwd cmd all_args

let cmake_configure ?(_cwd=None) ?(env=None) source_dir build_dir options =
  makedirs [build_dir];
  let cmd = "cmake" in
  let args = [source_dir] @ options in
  run_command ~cwd:(Some build_dir) ?env cmd args

let cmake_build ?(_cwd=None) ?(env=None) ?(jobs=1) build_dir =
  let cmd = "cmake" in
  let args = ["--build"; "."; "--parallel"; string_of_int jobs] in
  run_command ~cwd:(Some build_dir) ?env cmd args

let cmake_install ?(_cwd=None) build_dir install_prefix =
  let cmd = "cmake" in
  let args = ["--install"; "."; "--prefix"; install_prefix] in
  run_command ~cwd:(Some build_dir) cmd args

let remove_files_matching patterns base_dir =
  List.iter (fun pattern ->
    let cmd = "find" in
    let args = [base_dir; "-name"; pattern; "-delete"] in
    try run_command cmd args
    with Command_failed _ -> log_info ("No files matching pattern: " ^ pattern)
  ) patterns

let zip_directory source_dir dest_file =
  log_info ("Zipping: " ^ source_dir ^ " -> " ^ dest_file);
  let cmd = "zip" in
  let args = ["-r"; dest_file; "."] in
  run_command ~cwd:(Some source_dir) cmd args