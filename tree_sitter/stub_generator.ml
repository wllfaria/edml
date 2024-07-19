let c_headers = "#include <tree_sitter/api.h>"

let main () =
  let ml_out = open_out "tree_sitter_generated.ml" in
  let c_out = open_out "tree_sitter_stubs.c" in
  let ml_fmt = Format.formatter_of_out_channel ml_out in
  let c_fmt = Format.formatter_of_out_channel c_out in
  Format.fprintf c_fmt "%s\n" c_headers;
  let prefix = "ts" in
  Cstubs.write_c c_fmt ~prefix (module Tree_sitter.Bindings);
  Cstubs.write_ml ml_fmt ~prefix (module Tree_sitter.Bindings);
  (* Cstubs.Types.write_c c_fmt (module Tree_sitter.Enums); *)
  Format.pp_print_flush ml_fmt ();
  Format.pp_print_flush c_fmt ();
  close_out ml_out;
  close_out c_out
;;

let () = main ()
