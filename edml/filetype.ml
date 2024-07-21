open Core

type filetype =
  | Ocaml
  | OcamlInterface
  | PlainText
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
     | _ -> PlainText)
  | None -> PlainText
;;
