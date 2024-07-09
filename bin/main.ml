open Edml

let () =
  let filename = Sys.argv.(1) in
  (* Terminal.enable_raw_mode (); *)
  let content = Fs.open_file filename in
  let _text_object = Text_object.make_text_object content in
  let _buffer = Buffer.make_buffer 10 10 in
  ()
;;
(* Terminal.disable_raw_mode () *)
