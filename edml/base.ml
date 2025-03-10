open Core
open Languages
open Assertions
open Types

type direction =
  | Horizontal
  | Vertical
[@@deriving eq, show { with_path = false }]

type pane_tree =
  | Single of Pane.pane
  | Split of pane_branch
[@@deriving eq, show { with_path = false }]

and pane_branch =
  { direction : direction
  ; ratios : float list
  ; panes : pane_tree list
  }
[@@deriving eq, show { with_path = false }]

type tab =
  { panes : pane_tree
  ; active_pane : int
  }
[@@deriving eq, show { with_path = false }]

type editor =
  { tabs : tab list
  ; mode : mode
  ; quitting : bool
  ; buffers : Text_buffer.text_buffer list
  ; active_tab : int
  ; parsers : (language_id, Tree_sitter.ts_parser) Hashtbl.t [@opaque]
  }
[@@deriving show { with_path = false }]

let make_initial_text_object path =
  (match Fs.maybe_read_file path with
   | Some content -> content
   | None -> "")
  |> Text_object.make
  |> ref
;;

let make_pane buffer_id =
  let pane_id = Utils.next_id ~id_ref:Utils.pane_id in
  Pane.make ~buffer_id ~id:pane_id
;;

let make_buffer text_object path language_id tree matches =
  let buffer_id = Utils.next_id ~id_ref:Utils.buffer_id in
  Text_buffer.make text_object buffer_id path tree language_id matches
;;

let maybe_add_parser parsers filetype =
  match parser_from_filetype filetype with
  | Some (parser, lang) ->
    (match Hashtbl.find parsers lang with
     | Some _ -> ()
     | None ->
       let _ = Hashtbl.add parsers ~key:lang ~data:parser in
       ())
  | None -> ()
;;

let init path =
  let text_object = make_initial_text_object path in
  let filetype = Filetype.filetype_of_filename path in
  let language_id = language_id_of_filetype filetype in
  let parsers = Hashtbl.create (module Language_id) in
  maybe_add_parser parsers filetype;
  let tree, matches = maybe_parse_tree text_object parsers filetype language_id in
  let buffer = make_buffer text_object path language_id tree matches in
  let pane = make_pane buffer.id in
  let tab = { panes = Single pane; active_pane = pane.id } in
  { buffers = [ buffer ]
  ; tabs = [ tab ]
  ; active_tab = 0
  ; mode = Normal
  ; parsers
  ; quitting = false
  }
;;

let rec find_pane tree id =
  match tree with
  | Single p when p.id = id -> Some p
  | Split s -> List.find_map s.panes ~f:(fun t -> find_pane t id)
  | Single _ -> None
;;

let get_focused_pane editor =
  let tab = List.nth_exn editor.tabs editor.active_tab in
  let pane = find_pane tab.panes tab.active_pane in
  match pane with
  | Some p -> p
  | None -> unreachable ()
;;
