let make_c_headers root =
  let path = root ^ "/languages" in
  let prefix = "tree-sitter-" in
  let ts_header = "#include <tree_sitter/api.h>" in
  let dir = Sys.readdir path in
  Array.map (fun lang -> path ^ "/" ^ lang ^ "/" ^ prefix ^ lang ^ ".h") dir
  |> Array.fold_left
       (fun acc header -> acc ^ "\n" ^ "#include \"" ^ header ^ "\"")
       ts_header
;;

let rec find_upwards ~path ~needle =
  try
    let files = Sys.readdir path in
    let has_needle = Array.exists (fun item -> item = needle) files in
    if has_needle
    then path
    else (
      let splitted = String.split_on_char '/' path in
      match List.rev splitted with
      | [] | [ _ ] -> failwith "no root found"
      | _ :: rest ->
        let parent_path = String.concat "/" (List.rev rest) in
        find_upwards ~path:parent_path ~needle)
  with
  | Sys_error _ -> failwith "no root found"
;;

let main () =
  let cwd = Sys.getcwd () in
  let needle = "dune-project" in
  let root = find_upwards ~path:cwd ~needle in
  let headers = make_c_headers root in
  let ml_out = open_out "tree_sitter_generated.ml" in
  let c_out = open_out "tree_sitter_stubs.c" in
  let ml_fmt = Format.formatter_of_out_channel ml_out in
  let c_fmt = Format.formatter_of_out_channel c_out in
  Format.fprintf c_fmt "%s\n" headers;
  let prefix = "ts" in
  Cstubs.write_c c_fmt ~prefix (module Tree_sitter.Bindings);
  Cstubs.write_ml ml_fmt ~prefix (module Tree_sitter.Bindings);
  Format.pp_print_flush ml_fmt ();
  Format.pp_print_flush c_fmt ();
  close_out ml_out;
  close_out c_out
;;

let () = main ()
