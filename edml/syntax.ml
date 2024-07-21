open Core
open Assertions
open Filetype

type language_id =
  | Ocaml
  | Javascript
[@@deriving sexp, compare, hash]

module Language_id = struct
  module T = struct
    type t = language_id [@@deriving sexp, compare, hash]
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
