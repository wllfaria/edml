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
  let base_path = "/home/wiru/code/edml/languages" in
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

let query_matches_map_of_list (matches : Tree_sitter.query_match list) map =
  List.iter matches ~f:(fun m ->
    List.iter m.captures ~f:(fun c ->
      let start_byte = c.node.range.start_byte in
      let end_byte = c.node.range.end_byte in
      let _ = Hashtbl.add map ~key:(start_byte, end_byte) ~data:"lol" in
      ()));
  map
;;

let maybe_parse_tree text_object parsers filetype language_id =
  let map = Hashtbl.create (module IntTuple) in
  match Hashtbl.find parsers language_id with
  | None -> None, map
  | Some parser ->
    let source = Text_object.to_string !text_object in
    (match Tree_sitter.ts_parser_parse_string parser None source with
     | None -> None, map
     | Some tree ->
       (match language_of_language_id language_id with
        | None -> None, map
        | Some language ->
          (match Tree_sitter.ts_parser_set_language parser language with
           | false -> None, map
           | _ ->
             (match load_query filetype with
              | None -> None, map
              | Some query ->
                (match Tree_sitter.ts_query_new language query with
                 | Error _ -> None, map
                 | Ok query ->
                   let cursor = Tree_sitter.ts_query_cursor_new () in
                   let root_node = Tree_sitter.ts_tree_root_node tree in
                   let matches =
                     Tree_sitter.ts_query_cursor_matches cursor query root_node
                   in
                   Logger.log ~level:Error @@ Format.sprintf "%d" @@ List.length matches;
                   let matches_map = query_matches_map_of_list matches map in
                   Some tree, matches_map)))))
;;
