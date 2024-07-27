open Core
open Assertions
open Filetype
open Types

type text_buffer =
  { text_object : Text_object.t ref
  ; id : int
  ; filetype : filetype
  ; language_id : language_id
  ; syntax_tree : Tree_sitter.ts_tree option [@opaque]
  ; matches : match_map [@opaque]
  }
[@@deriving show { with_path = false }]

let get_filename path =
  let parts = String.split ~on:'/' path in
  assert_true (List.length parts > 0) "cannot get filename from empty path";
  List.last_exn parts
;;

let make text_object id filepath syntax_tree language_id (matches : match_map) =
  let filename = get_filename filepath in
  let filetype = filetype_of_filename filename in
  { text_object; id; filetype; language_id; syntax_tree; matches }
;;
