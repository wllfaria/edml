open Utils
open Assertions
open Types
open Bindings
module TS = Bindings (Tree_sitter_generated)

type ts_language = Types.ts_language
type ts_parser = Types.ts_parser
type ts_tree = Types.ts_tree
type ts_query = Types.ts_query
type ts_query_cursor = Types.ts_query_cursor
type ts_lookahead_iterator = Types.ts_lookahead_iterator
type ts_range = Types.ts_range

let ts_parser_new () =
  let parser = TS.ts_parser_new () in
  Gc.finalise (fun p -> TS.ts_parser_delete p) parser;
  parser
;;

let ts_parser_language = TS.ts_parser_language
let ts_parser_set_language = TS.ts_parser_set_language

let ts_parser_set_included_ranges parser ranges =
  let open Ctypes in
  let count = Unsigned.UInt32.of_int @@ List.length ranges in
  let c_ranges = CArray.of_list Types.ts_range (List.map ts_range_of_range ranges) in
  TS.ts_parser_set_included_ranges parser (CArray.start c_ranges) count
;;

let ts_parser_included_ranges parser =
  let open Ctypes in
  let counter = allocate uint32_t @@ uint32_of_int 0 in
  let ts_ranges = TS.ts_parser_included_ranges parser counter in
  let counter = uint32_to_int !@counter in
  let ranges =
    CArray.to_list @@ CArray.from_ptr ts_ranges counter
    |> List.map (fun r ->
      let start_byte = uint32_to_int @@ getf r Types.start_byte in
      let end_byte = uint32_to_int @@ getf r Types.end_byte in
      let start_point = ts_point_to_point @@ getf r Types.start_point in
      let end_point = ts_point_to_point @@ getf r Types.end_point in
      { start_byte; end_byte; start_point; end_point })
  in
  ranges, counter
;;

let ts_parser_parse parser tree input =
  let ts_input = ts_input_of_input input in
  TS.ts_parser_parse parser tree ts_input
;;

let ts_parser_parse_string parser tree code =
  let len = uint32_of_int @@ String.length code in
  TS.ts_parser_parse_string parser tree code len
;;

let ts_parser_parse_string_encoding parser tree code encoding =
  let len = uint32_of_int @@ String.length code in
  let encoding = uint64_of_int @@ ts_input_encoding_of_encoding encoding in
  TS.ts_parser_parse_string_encoding parser tree code len encoding
;;

let ts_parser_reset = TS.ts_parser_reset

let ts_parser_set_timeout_micros parser timeout =
  assert_true (timeout >= 0) "invalid timeout: must be a positive integer";
  let timeout = uint64_of_int timeout in
  TS.ts_parser_set_timeout_micros parser timeout
;;

let ts_parser_timeout_micros parser = uint64_to_int @@ TS.ts_parser_timeout_micros parser

(* ======================================== *
 *            THIS PART IS A WIP            *
 * ======================================== *)
let ts_parser_set_cancellation_flag = TS.ts_parser_set_cancellation_flag
let ts_parser_cancellation_flag = TS.ts_parser_cancellation_flag
let ts_parser_set_logger = TS.ts_parser_set_logger
let ts_parser_logger = TS.ts_parser_logger
let ts_parser_print_dot_graphs = TS.ts_parser_print_dot_graphs
let ts_tree_copy = TS.ts_tree_copy
let ts_tree_delete = TS.ts_tree_delete
let ts_tree_root_node = TS.ts_tree_root_node
let ts_tree_root_node_with_offset = TS.ts_tree_root_node_with_offset
let ts_tree_language = TS.ts_tree_language
let ts_tree_included_ranges = TS.ts_tree_included_ranges
let ts_tree_edit = TS.ts_tree_edit
let ts_tree_get_changed_ranges = TS.ts_tree_get_changed_ranges
let ts_tree_print_dot_graph = TS.ts_tree_print_dot_graph
let ts_node_type = TS.ts_node_type
let ts_node_symbol = TS.ts_node_symbol
let ts_node_language = TS.ts_node_language
let ts_node_grammar_type = TS.ts_node_grammar_type
let ts_node_grammar_symbol = TS.ts_node_grammar_symbol
let ts_node_start_byte = TS.ts_node_start_byte
let ts_node_start_point = TS.ts_node_start_point
let ts_node_end_byte = TS.ts_node_end_byte
let ts_node_end_point = TS.ts_node_end_point
let ts_node_string = TS.ts_node_string
let ts_node_is_null = TS.ts_node_is_null
let ts_node_is_named = TS.ts_node_is_named
let ts_node_is_missing = TS.ts_node_is_missing
let ts_node_is_extra = TS.ts_node_is_extra
let ts_node_has_changes = TS.ts_node_has_changes
let ts_node_has_error = TS.ts_node_has_error
let ts_node_is_error = TS.ts_node_is_error
let ts_node_parse_state = TS.ts_node_parse_state
let ts_node_next_parse_state = TS.ts_node_next_parse_state
let ts_node_parent = TS.ts_node_parent
let ts_node_child_containing_descendant = TS.ts_node_child_containing_descendant
let ts_node_child = TS.ts_node_child
let ts_node_field_name_for_child = TS.ts_node_field_name_for_child
let ts_node_child_count = TS.ts_node_child_count
let ts_node_named_child = TS.ts_node_named_child
let ts_node_named_child_count = TS.ts_node_named_child_count
let ts_node_child_by_field_name = TS.ts_node_child_by_field_name
let ts_node_child_by_field_id = TS.ts_node_child_by_field_id
let ts_node_next_sibling = TS.ts_node_next_sibling
let ts_node_prev_sibling = TS.ts_node_prev_sibling
let ts_node_next_named_sibling = TS.ts_node_next_named_sibling
let ts_node_prev_named_sibling = TS.ts_node_prev_named_sibling
let ts_node_first_child_for_byte = TS.ts_node_first_child_for_byte
let ts_node_first_named_child_for_byte = TS.ts_node_first_named_child_for_byte
let ts_node_descendant_count = TS.ts_node_descendant_count
let ts_node_descendant_for_byte_range = TS.ts_node_descendant_for_byte_range
let ts_node_descendant_for_point_range = TS.ts_node_descendant_for_point_range
let ts_node_named_descendant_for_byte_range = TS.ts_node_named_descendant_for_byte_range
let ts_node_named_descendant_for_point_range = TS.ts_node_named_descendant_for_point_range
let ts_node_edit = TS.ts_node_edit
let ts_node_eq = TS.ts_node_eq
let ts_tree_cursor_new = TS.ts_tree_cursor_new
let ts_tree_cursor_delete = TS.ts_tree_cursor_delete
let ts_tree_cursor_reset = TS.ts_tree_cursor_reset
let ts_tree_cursor_reset_to = TS.ts_tree_cursor_reset_to
let ts_tree_cursor_current_node = TS.ts_tree_cursor_current_node
let ts_tree_cursor_current_field_name = TS.ts_tree_cursor_current_field_name
let ts_tree_cursor_current_field_id = TS.ts_tree_cursor_current_field_id
let ts_tree_cursor_goto_parent = TS.ts_tree_cursor_goto_parent
let ts_tree_cursor_goto_next_sibling = TS.ts_tree_cursor_goto_next_sibling
let ts_tree_cursor_goto_previous_sibling = TS.ts_tree_cursor_goto_previous_sibling
let ts_tree_cursor_goto_first_child = TS.ts_tree_cursor_goto_first_child
let ts_tree_cursor_goto_last_child = TS.ts_tree_cursor_goto_last_child
let ts_tree_cursor_goto_descendant = TS.ts_tree_cursor_goto_descendant
let ts_tree_cursor_current_descendant_index = TS.ts_tree_cursor_current_descendant_index
let ts_tree_cursor_current_depth = TS.ts_tree_cursor_current_depth
let ts_tree_cursor_goto_first_child_for_byte = TS.ts_tree_cursor_goto_first_child_for_byte

let ts_tree_cursor_goto_first_child_for_point =
  TS.ts_tree_cursor_goto_first_child_for_point
;;

let ts_tree_cursor_copy = TS.ts_tree_cursor_copy

let ts_query_new language code =
  let open Ctypes in
  let len = uint32_of_int @@ String.length code in
  let err_offset = allocate_n uint32_t ~count:1 in
  let err_type = allocate_n uint32_t ~count:1 in
  let result = TS.ts_query_new language code len err_offset err_type in
  if Ctypes.(to_voidp result) = Ctypes.null
  then (
    let err_type =
      match uint32_to_int !@err_type with
      | 0 -> TSQueryErrorNone
      | 1 -> TSQueryErrorSyntax
      | 2 -> TSQueryErrorNodeType
      | 3 -> TSQueryErrorField
      | 4 -> TSQueryErrorCapture
      | 5 -> TSQueryErrorStructure
      | 6 -> TSQueryErrorLanguage
      | _ -> unreachable ()
    in
    Error (uint32_to_int !@err_offset, err_type))
  else Ok result
;;

let ts_query_delete = TS.ts_query_delete
let ts_query_pattern_count = TS.ts_query_pattern_count
let ts_query_capture_count = TS.ts_query_capture_count
let ts_query_string_count = TS.ts_query_string_count
let ts_query_start_byte_for_pattern = TS.ts_query_start_byte_for_pattern
let ts_query_predicates_for_pattern = TS.ts_query_predicates_for_pattern
let ts_query_is_pattern_rooted = TS.ts_query_is_pattern_rooted
let ts_query_is_pattern_non_local = TS.ts_query_is_pattern_non_local
let ts_query_is_pattern_guaranteed_at_step = TS.ts_query_is_pattern_guaranteed_at_step
let ts_query_capture_name_for_id = TS.ts_query_capture_name_for_id
let ts_query_capture_quantifier_for_id = TS.ts_query_capture_quantifier_for_id
let ts_query_string_value_for_id = TS.ts_query_string_value_for_id
let ts_query_disable_capture = TS.ts_query_disable_capture
let ts_query_disable_pattern = TS.ts_query_disable_pattern
let ts_query_cursor_new = TS.ts_query_cursor_new
let ts_query_cursor_delete = TS.ts_query_cursor_delete
let ts_query_cursor_exec = TS.ts_query_cursor_exec
let ts_query_cursor_did_exceed_match_limit = TS.ts_query_cursor_did_exceed_match_limit
let ts_query_cursor_match_limit = TS.ts_query_cursor_match_limit
let ts_query_cursor_set_match_limit = TS.ts_query_cursor_set_match_limit
let ts_query_cursor_set_byte_range = TS.ts_query_cursor_set_byte_range
let ts_query_cursor_set_point_range = TS.ts_query_cursor_set_point_range
let ts_query_cursor_next_match = TS.ts_query_cursor_next_match
let ts_query_cursor_remove_match = TS.ts_query_cursor_remove_match
let ts_query_cursor_next_capture = TS.ts_query_cursor_next_capture
let ts_query_cursor_set_max_start_depth = TS.ts_query_cursor_set_max_start_depth
let ts_language_copy = TS.ts_language_copy
let ts_language_delete = TS.ts_language_delete
let ts_language_symbol_count = TS.ts_language_symbol_count
let ts_language_state_count = TS.ts_language_state_count
let ts_language_symbol_name = TS.ts_language_symbol_name
let ts_language_symbol_for_name = TS.ts_language_symbol_for_name
let ts_language_field_count = TS.ts_language_field_count
let ts_language_field_name_for_id = TS.ts_language_field_name_for_id
let ts_language_field_id_for_name = TS.ts_language_field_id_for_name
let ts_language_symbol_type = TS.ts_language_symbol_type
let ts_language_version = TS.ts_language_version
let ts_language_next_state = TS.ts_language_next_state
let ts_lookahead_iterator_new = TS.ts_lookahead_iterator_new
let ts_lookahead_iterator_delete = TS.ts_lookahead_iterator_delete
let ts_lookahead_iterator_reset_state = TS.ts_lookahead_iterator_reset_state
let ts_lookahead_iterator_reset = TS.ts_lookahead_iterator_reset
let ts_lookahead_iterator_language = TS.ts_lookahead_iterator_language
let ts_lookahead_iterator_next = TS.ts_lookahead_iterator_next
let ts_lookahead_iterator_current_symbol = TS.ts_lookahead_iterator_current_symbol

let ts_lookahead_iterator_current_symbol_name =
  TS.ts_lookahead_iterator_current_symbol_name
;;

let tree_sitter_javascript = TS.tree_sitter_javascript
let tree_sitter_ocaml = TS.tree_sitter_ocaml
let tree_sitter_ocaml_interface = TS.tree_sitter_ocaml_interface
let tree_sitter_ocaml_type = TS.tree_sitter_ocaml_type
