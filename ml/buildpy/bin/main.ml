open Printf
open Buildpy

let version = ref "3.12.9"
let config = ref "static_max"
let packages = ref []
let opts = ref []
let jobs = ref 4
let optimize = ref false
let reset = ref false
let debug = ref false
let use_git = ref false

let add_package pkg = packages := pkg :: !packages
let add_opt opt = opts := opt :: !opts


let build_command () =
  printf "Environment: platform=%s, arch=%s\n"
    (match Sys.os_type with "Unix" -> "unix" | _ -> "other")
    (let ic = Unix.open_process_in "uname -m 2>/dev/null || echo unknown" in
     let arch = String.trim (input_line ic) in
     let _ = Unix.close_process_in ic in arch);

  printf "Build options: version=%s, config=%s, optimize=%b, jobs=%d\n"
    !version !config !optimize !jobs;

  let builder = Builder.create_python_builder !version !config in
  let builder = { builder with
    config_options = !opts @ builder.config_options;
    packages = !packages;
    optimize = !optimize;
    use_git = !use_git;
    jobs = !jobs;
  } in

  if !reset then Project.reset builder.project;

  Builder.process builder

let config_command () =
  printf "Available configurations:\n";
  printf "  static_max  - Maximum static build\n";
  printf "  static_mid  - Medium static build\n";
  printf "  static_min  - Minimal static build\n";
  printf "  shared_max  - Maximum shared build\n";
  printf "  shared_mid  - Medium shared build\n";
  printf "\n";
  printf "Current config: %s\n" !config;

  let config_obj = Config.create_config !config !version in
  let configured = Config.configure_for_platform_and_version config_obj in
  let setup_content = Config.generate_setup_local configured in
  printf "\nSetup.local preview:\n%s\n" (String.sub setup_content 0 (min 500 (String.length setup_content)))

let deps_command () =
  printf "Installing dependencies...\n";
  Dependency.install_dependencies ();
  printf "Dependencies installed successfully!\n"

let speclist = [
  ("-v", Arg.Set_string version, " Python version (default: 3.12.9)");
  ("-c", Arg.Set_string config, " Build configuration (default: static_max)");
  ("-p", Arg.String add_package, " Add Python package");
  ("-o", Arg.String add_opt, " Add configure option");
  ("-j", Arg.Set_int jobs, " Number of build jobs (default: 4)");
  ("-O", Arg.Set optimize, " Optimize build");
  ("-r", Arg.Set reset, " Reset build directory");
  ("-d", Arg.Set debug, " Debug mode");
  ("-g", Arg.Set use_git, " Use git to download Python");
]

let commands = [
  ("build", build_command, "Build Python from source");
  ("config", config_command, "Show configuration information");
  ("deps", deps_command, "Install dependencies only");
]

let print_help () =
  printf "buildpy - Build Python from source\n\n";
  printf "Usage: buildpy <command> [options]\n\n";
  printf "Commands:\n";
  List.iter (fun (name, _, desc) ->
    printf "  %-10s %s\n" name desc
  ) commands;
  printf "\nOptions:\n";
  List.iter (fun (flag, _, desc) ->
    printf "  %-10s %s\n" flag desc
  ) speclist;
  printf "\nExamples:\n";
  printf "  buildpy build -v 3.12.9 -c static_max\n";
  printf "  buildpy build -v 3.11.0 -c shared_max -p numpy,scipy\n";
  printf "  buildpy config\n";
  printf "  buildpy deps\n"

let () =
  if Array.length Sys.argv < 2 then (
    print_help ();
    exit 1
  );

  let command = Sys.argv.(1) in
  let remaining_args = Array.sub Sys.argv 2 (Array.length Sys.argv - 2) in

  match List.find_opt (fun (name, _, _) -> name = command) commands with
  | Some (_, cmd_func, _) ->
    let args = Array.to_list remaining_args in
    let rec parse_args args =
      match args with
      | "-v" :: value :: rest -> version := value; parse_args rest
      | "-c" :: value :: rest -> config := value; parse_args rest
      | "-p" :: value :: rest -> add_package value; parse_args rest
      | "-o" :: value :: rest -> add_opt value; parse_args rest
      | "-j" :: value :: rest -> jobs := int_of_string value; parse_args rest
      | "-O" :: rest -> optimize := true; parse_args rest
      | "-r" :: rest -> reset := true; parse_args rest
      | "-d" :: rest -> debug := true; parse_args rest
      | "-g" :: rest -> use_git := true; parse_args rest
      | [] -> ()
      | _ :: rest -> parse_args rest
    in
    parse_args args;
    cmd_func ()
  | None ->
    printf "Unknown command: %s\n\n" command;
    print_help ();
    exit 1