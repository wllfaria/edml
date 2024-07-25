open Core

type filetype =
  | Ocaml
  | OcamlInterface
  | PlainText
  | Javascript
[@@deriving eq, show { with_path = false }]

let extension_of_filename filename =
  let parts = String.split ~on:'.' filename in
  let len = List.length parts in
  if len > 1 then Some (List.last_exn parts) else None
;;

let filetype_of_filename filename =
  match extension_of_filename filename with
  | Some extension ->
    (match extension with
     | "ml" -> Ocaml
     | "mli" -> OcamlInterface
     | "js" -> Javascript
     | "cjs" -> Javascript
     | _ -> PlainText)
  | None -> PlainText
;;

let string_of_filetype filetype =
  match filetype with
  | Ocaml -> "ocaml"
  | OcamlInterface -> "ocaml_interface"
  | Javascript -> "javascript"
  | PlainText -> "plain_text"
;;
