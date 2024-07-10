(* open Edml *)

(* let filename = Sys.argv.(1) in *)
(* Ansi.Terminal.enter_alternate_screen (); *)
(* let _ = Fs.read_file filename in *)

let () =
  Ansi.Terminal.enable_raw_mode ();
  Ansi.Terminal.clear_screen ();
  print_endline "lol";
  let result = Ansi.Event.read () in
  print_endline result
;;

(* Ansi.Terminal.leave_alternate_screen () *)
