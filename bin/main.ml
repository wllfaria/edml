open Edml

let main env filename =
  let _stdin = Eio.Stdenv.stdin env in
  let _stdout = Eio.Stdenv.stdout env in
  let _stderr = Eio.Stdenv.stderr env in
  let content = Fs.open_file filename in
  let text_object = Text_object.make_text_object content in
  Fmt.pr "%a" Text_object.pp_text_object text_object
;;

let () =
  let filename = Sys.argv.(1) in
  Eio_main.run @@ fun env -> main env filename
;;
