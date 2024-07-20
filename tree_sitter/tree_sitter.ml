open Ctypes

module Types = struct
  type ts_language = unit ptr
  type ts_parser = unit ptr
  type ts_tree = unit ptr
  type ts_range
  type ts_point
  type ts_input
  type ts_node

  (* Opaque *)
  let ts_parser : ts_parser typ = ptr void
  let ts_language : ts_language typ = ptr void
  let ts_tree : ts_tree typ = ptr void

  (* TSPoint *)
  let ts_point : ts_point structure typ = structure "TSPoint"
  let row = field ts_point "row" uint32_t
  let column = field ts_point "column" uint32_t
  let () = seal ts_point

  (* TSRange *)
  let ts_range : ts_range structure typ = structure "TSRange"
  let start_point = field ts_range "start_point" ts_point
  let end_point = field ts_range "end_point" ts_point
  let start_byte = field ts_range "start_byte" uint32_t
  let end_byte = field ts_range "end_byte" uint32_t
  let () = seal ts_range

  (* TSInput *)
  let input : ts_input structure typ = structure "TSInput"
  let payload = field input "payload" (ptr void)

  let read_fn =
    ptr void @-> uint32_t @-> ts_point @-> ptr uint32_t @-> returning (ptr char)
  ;;

  let read = field input "read" (static_funptr read_fn)
  let encoding = field input "encoding" int
  let () = seal input

  (* TSNode *)
  let ts_node : ts_node structure typ = structure "TSNode"
  let context = field ts_node "context" (array 4 uint32_t)
  let id = field ts_node "id" (ptr void)
  let tree = field ts_node "tree" (ptr void)
  let () = seal ts_node
end

module Bindings (F : Ctypes.FOREIGN) = struct
  open F
  open Types

  let ts_parser_new                    = foreign "ts_parser_new" (void @-> returning ts_parser)
  let ts_parser_delete                 = foreign "ts_parser_delete" (ts_parser @-> returning void)
  let ts_parser_language               = foreign "ts_parser_language" (ts_parser @-> returning ts_language)
  let ts_parser_set_language           = foreign "ts_parser_set_language" (ts_parser @-> ts_language @-> returning bool)
  let ts_parser_set_included_ranges    = foreign "ts_parser_set_included_ranges" (ts_parser @-> ptr ts_range @-> uint32_t @-> returning bool)
  let ts_parser_included_ranges        = foreign "ts_parser_included_ranges" (ts_parser @-> ptr uint32_t @-> returning (ptr ts_range))
  let ts_parser_parse                  = foreign "ts_parser_parse" (ts_parser @-> ts_tree @-> input @-> returning ts_tree)
  let ts_parser_parse_string           = foreign "ts_parser_parse_string" (ts_parser @-> ts_tree @-> string @-> uint32_t @-> returning ts_tree)

  let ts_tree_root_node                = foreign "ts_tree_root_node" (ts_tree @-> returning ts_node)
  let ts_node_type                     = foreign "ts_node_type" (ts_node @-> returning string)
  let ts_node_start_byte               = foreign "ts_node_start_byte" (ts_node @-> returning uint32_t)
  let ts_node_end_byte                 = foreign "ts_node_end_byte" (ts_node @-> returning uint32_t)
  let ts_node_child_count              = foreign "ts_node_child_count" (ts_node @-> returning uint32_t)
  let ts_node_child                    = foreign "ts_node_child" (ts_node @-> uint32_t @-> returning ts_node)

  let tree_sitter_javascript           = foreign "tree_sitter_javascript" (void @-> returning ts_language)
end
[@@ocamlformat "disable"]
