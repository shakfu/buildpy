(** An example of an application *)

let usage_msg = "buildpy [-verbose] <file1> [<file2>] ... -o <output>"
let verbose = ref false
let input_files = ref []
let output_file = ref ""

let anon_fun filename = 
  input_files := filename :: !input_files

let speclist =
  [
    ("-verbose", Arg.Set verbose, "Output debug information");
    ("-o", Arg.Set_string output_file, "Set output file name");
  ]

let () =
  Arg.parse speclist anon_fun usage_msg;;
  Printf.printf "output_file: %s \n" !output_file;;