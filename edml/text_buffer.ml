open Core
open Assertions
open Filetype

type text_buffer =
  { text_object : Text_object.t ref
  ; id : int
  ; filetype : filetype
  }
[@@deriving eq, show { with_path = false }]

let get_filename path =
  let parts = String.split ~on:'/' path in
  assert_true (List.length parts > 0) "cannot get filename from empty path";
  List.last_exn parts
;;

let make text_object id filepath =
  let filename = get_filename filepath in
  let filetype = filetype_of_filename filename in
  { text_object; id; filetype }
;;
