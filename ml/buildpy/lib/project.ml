
type project = {
  cwd: string;
  build: string;
  downloads: string;
  src: string;
  install: string;
}

let create_project () =
  let cwd = Sys.getcwd () in
  let build = Filename.concat cwd "build" in
  let downloads = Filename.concat build "downloads" in
  let src = Filename.concat build "src" in
  let install = Filename.concat build "install" in
  { cwd; build; downloads; src; install }

let setup project =
  Shell.makedirs [project.build; project.downloads; project.src; project.install]

let clean project =
  if Sys.file_exists project.src then (
    Shell.log_info ("Cleaning src directory: " ^ project.src);
    let cmd = "rm" in
    let args = ["-rf"; project.src] in
    Shell.run_command cmd args
  )

let reset project =
  if Sys.file_exists project.build then (
    Shell.log_info ("Resetting build directory: " ^ project.build);
    let cmd = "rm" in
    let args = ["-rf"; project.build] in
    Shell.run_command cmd args
  )