open Core
open Assertions
open Filetype
open Types

module Language_id = struct
  module T = struct
    type t = language_id [@@deriving sexp, compare, hash, eq, show { with_path = false }]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

let parser_from_filetype (filetype : filetype) =
  let parser = Tree_sitter.ts_parser_new () in
  match filetype with
  | Ocaml ->
    let language = Tree_sitter.tree_sitter_ocaml () in
    let ok = Tree_sitter.ts_parser_set_language parser language in
    if not ok then panic "failed to set parser language";
    Some (parser, Ocaml)
  | OcamlInterface ->
    let language = Tree_sitter.tree_sitter_ocaml () in
    let ok = Tree_sitter.ts_parser_set_language parser language in
    if not ok then panic "failed to set parser language";
    Some (parser, Ocaml)
  | Javascript ->
    let language = Tree_sitter.tree_sitter_javascript () in
    let ok = Tree_sitter.ts_parser_set_language parser language in
    if not ok then panic "failed to set parser language";
    Some (parser, Javascript)
  | PlainText -> None
;;

let string_of_language_id language =
  match language with
  | Ocaml -> "ocaml"
  | Javascript -> "javascript"
  | PlainText -> "plain_text"
;;

let language_id_of_filetype (filetype : filetype) =
  match filetype with
  | Ocaml -> Ocaml
  | OcamlInterface -> Ocaml
  | Javascript -> Javascript
  | PlainText -> PlainText
;;

let load_query filetype =
  (* TODO: remove this absolute path lol *)
  let base_path = "/Users/wiru/code/edml/languages" in
  let language = string_of_language_id @@ language_id_of_filetype filetype in
  let query_kind = string_of_filetype filetype in
  let path = Fs.join_paths [ base_path; language; query_kind ^ ".scm" ] in
  print_endline path;
  Fs.maybe_read_file path
;;

let query_from_filetype (filetype : filetype) =
  match filetype with
  | Javascript -> load_query filetype
  | OcamlInterface -> load_query filetype
  | Ocaml -> load_query filetype
  | PlainText -> None
;;

let language_of_language_id language_id =
  match language_id with
  | Javascript -> Option.return @@ Tree_sitter.tree_sitter_javascript ()
  | Ocaml -> Option.return @@ Tree_sitter.tree_sitter_ocaml ()
  | PlainText -> None
;;
