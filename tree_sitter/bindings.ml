open Ctypes

module Types = struct
  (* Opaque *)
  type ts_language = unit ptr
  type ts_parser = unit ptr [@@deriving eq, show]
  type ts_tree = unit ptr
  type ts_query = unit ptr
  type ts_query_cursor = unit ptr
  type ts_lookahead_iterator = unit ptr

  (* Non Opaque *)
  type ts_point
  type ts_range
  type ts_input
  type ts_node
  type ts_logger
  type ts_input_edit
  type ts_tree_cursor
  type ts_query_capture
  type ts_query_match
  type ts_query_predicate_step

  (* Opaque *)
  let ts_parser : ts_parser typ = ptr void
  let ts_language : ts_language typ = ptr void
  let ts_tree : ts_tree typ = ptr void
  let ts_query : ts_query typ = ptr void
  let ts_query_cursor : ts_query_cursor typ = ptr void
  let ts_lookahead_iterator : ts_lookahead_iterator typ = ptr void

  (* TSPoint *)
  let ts_point : ts_point structure typ = structure "TSPoint"
  let ts_point_row = field ts_point "row" uint32_t
  let ts_point_column = field ts_point "column" uint32_t
  let () = seal ts_point

  (* TSRange *)
  let ts_range : ts_range structure typ = structure "TSRange"
  let start_point = field ts_range "start_point" ts_point
  let end_point = field ts_range "end_point" ts_point
  let start_byte = field ts_range "start_byte" uint32_t
  let end_byte = field ts_range "end_byte" uint32_t
  let () = seal ts_range

  (* TSInput *)
  let ts_input : ts_input structure typ = structure "TSInput"
  let ts_input_payload = field ts_input "payload" (ptr void)

  let read_fn =
    ptr void @-> uint32_t @-> ts_point @-> ptr uint32_t @-> returning (ptr char)
  ;;

  let ts_input_read = field ts_input "read" (static_funptr read_fn)
  let ts_input_encoding = field ts_input "encoding" int
  let () = seal ts_input

  (* TSLogger *)
  let ts_logger : ts_logger structure typ = structure "TSLogger"
  let logger_payload = field ts_logger "payload" (ptr void)
  let log_fn = ptr void @-> uint64_t @-> string @-> returning void
  let log = field ts_logger "log" (static_funptr log_fn)
  let () = seal ts_logger

  (* TSInputEdit *)
  let ts_input_edit : ts_input_edit structure typ = structure "TSInputEdit"
  let input_edit_start_byte = field ts_input_edit "start_byte" uint32_t
  let input_edit_old_end_byte = field ts_input_edit "old_end_byte" uint32_t
  let input_edit_new_end_byte = field ts_input_edit "new_end_byte" uint32_t
  let input_edit_start_point = field ts_input_edit "start_point" ts_point
  let input_edit_old_end_point = field ts_input_edit "old_end_point" ts_point
  let input_edit_new_end_point = field ts_input_edit "new_endPoint" ts_point
  let () = seal ts_input_edit

  (* TSNode *)
  let ts_node : ts_node structure typ = structure "TSNode"
  let context = field ts_node "context" (array 4 uint32_t)
  let id = field ts_node "id" (ptr void)
  let tree = field ts_node "tree" (ptr void)
  let () = seal ts_node

  (* TSTreeCursor *)
  let ts_tree_cursor : ts_tree_cursor structure typ = structure "TSTreeCursor"
  let ts_tree_cursor_tree = field ts_tree_cursor "tree" (ptr void)
  let ts_tree_cursor_id = field ts_tree_cursor "id" (ptr void)
  let ts_tree_cursor_context = field ts_tree_cursor "context" (array 3 uint32_t)
  let () = seal ts_tree_cursor

  (*TSQueryCapture *)
  let ts_query_capture : ts_query_capture structure typ = structure "TSQueryCapture"
  let ts_query_capture_node = field ts_query_capture "node" ts_node
  let ts_query_capture_index = field ts_query_capture "index" uint32_t
  let () = seal ts_query_capture

  (* TSQueryMatch *)
  let ts_query_match : ts_query_match structure typ = structure "TSQueryMatch"
  let ts_query_match_id = field ts_query_match "id" uint32_t
  let ts_query_match_pattern_index = field ts_query_match "pattern_index" uint16_t
  let ts_query_match_capture_count = field ts_query_match "capture_count" uint16_t

  let ts_query_match_capture_captures =
    field ts_query_match "captures" (ptr ts_query_capture)
  ;;

  let () = seal ts_query_match

  (* TSQueryPredicateStep *)
  let ts_query_predicate_step : ts_query_predicate_step structure typ =
    structure "TSQueryPredicateStep"
  ;;

  let ts_query_predicate_step_type = field ts_query_predicate_step "type" uint32_t
  let ts_query_predicate_step_value_id = field ts_query_predicate_step "value_id" uint32_t
  let () = seal ts_query_predicate_step
end

module Bindings (F : Ctypes.FOREIGN) = struct
  open F
  open Types

  (* Parser *)
  let ts_parser_new                             = foreign "ts_parser_new" (void @-> returning ts_parser)
  let ts_parser_delete                          = foreign "ts_parser_delete" (ts_parser @-> returning void)
  let ts_parser_language                        = foreign "ts_parser_language" (ts_parser @-> returning ts_language)
  let ts_parser_set_language                    = foreign "ts_parser_set_language" (ts_parser @-> ts_language @-> returning bool)
  let ts_parser_set_included_ranges             = foreign "ts_parser_set_included_ranges" (ts_parser @-> ptr ts_range @-> uint32_t @-> returning bool)
  let ts_parser_included_ranges                 = foreign "ts_parser_included_ranges" (ts_parser @-> ptr uint32_t @-> returning (ptr ts_range))
  let ts_parser_parse                           = foreign "ts_parser_parse" (ts_parser @-> ts_tree @-> ts_input @-> returning ts_tree)
  let ts_parser_parse_string                    = foreign "ts_parser_parse_string" (ts_parser @-> ts_tree @-> string @-> uint32_t @-> returning ts_tree)
  let ts_parser_parse_string_encoding           = foreign "ts_parser_parse_string_encoding" (ts_parser @-> ts_tree @-> string @-> uint32_t @-> uint64_t @-> returning ts_tree)
  let ts_parser_reset                           = foreign "ts_parser_reset" (ts_parser @-> returning void)
  let ts_parser_set_timeout_micros              = foreign "ts_parser_set_timeout_micros" (ts_parser @-> uint64_t @-> returning void)
  let ts_parser_timeout_micros                  = foreign "ts_parser_timeout_micros" (ts_parser @-> returning uint64_t)
  let ts_parser_set_cancellation_flag           = foreign "ts_parser_set_cancellation_flag" (ts_parser @-> ptr size_t @-> returning void)
  let ts_parser_cancellation_flag               = foreign "ts_parser_cancellation_flag" (ts_parser @-> returning (ptr size_t))
  let ts_parser_set_logger                      = foreign "ts_parser_set_logger" (ts_parser @-> ts_logger @-> returning void)
  let ts_parser_logger                          = foreign "ts_parser_logger" (ts_parser @-> returning ts_logger)
  let ts_parser_print_dot_graphs                = foreign "ts_parser_print_dot_graphs" (ts_parser @-> int @-> returning void)

  (* Tree *)
  let ts_tree_copy                              = foreign "ts_tree_copy" (ts_tree @-> returning ts_tree)
  let ts_tree_delete                            = foreign "ts_tree_delete" (ts_tree @-> returning void)
  let ts_tree_root_node                         = foreign "ts_tree_root_node" (ts_tree @-> returning ts_node)
  let ts_tree_root_node_with_offset             = foreign "ts_tree_root_node_with_offset" (ts_tree @-> uint32_t @-> ts_point @-> returning ts_node)
  let ts_tree_language                          = foreign "ts_tree_language" (ts_tree @-> returning ts_language)
  let ts_tree_included_ranges                   = foreign "ts_tree_included_ranges" (ts_tree @-> ptr uint32_t @-> returning (ptr ts_range))
  let ts_tree_edit                              = foreign "ts_tree_edit" (ts_tree @-> ptr ts_input_edit @-> returning void)
  let ts_tree_get_changed_ranges                = foreign "ts_tree_get_changed_ranges" (ts_tree @-> ts_tree @-> ptr uint32_t @-> returning (ptr ts_range))
  let ts_tree_print_dot_graph                   = foreign "ts_tree_print_dot_graph" (ts_tree @-> int @-> returning void)

  (* Node *)
  let ts_node_type                              = foreign "ts_node_type" (ts_node @-> returning string)
  let ts_node_symbol                            = foreign "ts_node_symbol" (ts_node @-> returning uint16_t)
  let ts_node_language                          = foreign "ts_node_language" (ts_node @-> returning ts_language)
  let ts_node_grammar_type                      = foreign "ts_node_grammar_type" (ts_node @-> returning string)
  let ts_node_grammar_symbol                    = foreign "ts_node_grammar_symbol" (ts_node @-> returning uint16_t)
  let ts_node_start_byte                        = foreign "ts_node_start_byte" (ts_node @-> returning uint32_t)
  let ts_node_start_point                       = foreign "ts_node_start_point" (ts_node @-> returning ts_point)
  let ts_node_end_byte                          = foreign "ts_node_end_byte" (ts_node @-> returning uint32_t)
  let ts_node_end_point                         = foreign "ts_node_end_point" (ts_node @-> returning ts_point)
  let ts_node_string                            = foreign "ts_node_string" (ts_node @-> returning string)
  let ts_node_is_null                           = foreign "ts_node_is_null" (ts_node @-> returning bool)
  let ts_node_is_named                          = foreign "ts_node_is_named" (ts_node @-> returning bool)
  let ts_node_is_missing                        = foreign "ts_node_is_missing" (ts_node @-> returning bool)
  let ts_node_is_extra                          = foreign "ts_node_is_extra" (ts_node @-> returning bool)
  let ts_node_has_changes                       = foreign "ts_node_has_changes" (ts_node @-> returning bool)
  let ts_node_has_error                         = foreign "ts_node_has_error" (ts_node @-> returning bool)
  let ts_node_is_error                          = foreign "ts_node_is_error" (ts_node @-> returning bool)
  let ts_node_parse_state                       = foreign "ts_node_parse_state" (ts_node @-> returning uint16_t)
  let ts_node_next_parse_state                  = foreign "ts_node_next_parse_state" (ts_node @-> returning uint16_t)
  let ts_node_parent                            = foreign "ts_node_parent" (ts_node @-> returning ts_node)
  let ts_node_child_containing_descendant       = foreign "ts_node_child_containing_descendant" (ts_node @-> ts_node @-> returning ts_node)
  let ts_node_child                             = foreign "ts_node_child" (ts_node @-> uint32_t @-> returning ts_node)
  let ts_node_field_name_for_child              = foreign "ts_node_field_name_for_child" (ts_node @-> uint32_t @-> returning string)
  let ts_node_child_count                       = foreign "ts_node_child_count" (ts_node @-> returning uint32_t)
  let ts_node_named_child                       = foreign "ts_node_named_child" (ts_node @-> uint32_t @-> returning ts_node)
  let ts_node_named_child_count                 = foreign "ts_node_named_child_count" (ts_node @-> returning uint32_t)
  let ts_node_child_by_field_name               = foreign "ts_node_child_by_field_name" (ts_node @-> string @-> uint32_t @-> returning ts_node)
  let ts_node_child_by_field_id                 = foreign "ts_node_child_by_field_id" (ts_node @-> uint16_t @-> returning ts_node)
  let ts_node_next_sibling                      = foreign "ts_node_next_sibling" (ts_node @-> returning ts_node)
  let ts_node_prev_sibling                      = foreign "ts_node_prev_sibling" (ts_node @-> returning ts_node)
  let ts_node_next_named_sibling                = foreign "ts_node_next_named_sibling" (ts_node @-> returning ts_node)
  let ts_node_prev_named_sibling                = foreign "ts_node_prev_named_sibling" (ts_node @-> returning ts_node)
  let ts_node_first_child_for_byte              = foreign "ts_node_first_child_for_byte" (ts_node @-> uint32_t @-> returning ts_node)
  let ts_node_first_named_child_for_byte        = foreign "ts_node_first_named_child_for_byte" (ts_node @-> uint32_t @-> returning ts_node)
  let ts_node_descendant_count                  = foreign "ts_node_descendant_count" (ts_node @-> returning uint32_t)
  let ts_node_descendant_for_byte_range         = foreign "ts_node_descendant_for_byte_range" (ts_node @-> uint32_t @-> uint32_t @-> returning ts_node)
  let ts_node_descendant_for_point_range        = foreign "ts_node_descendant_for_point_range" (ts_node @-> ts_point @-> ts_point @-> returning ts_node)
  let ts_node_named_descendant_for_byte_range   = foreign "ts_node_named_descendant_for_byte_range" (ts_node @-> uint32_t @-> uint32_t @-> returning ts_node)
  let ts_node_named_descendant_for_point_range  = foreign "ts_node_named_descendant_for_point_range" (ts_node @-> ts_point @-> ts_point @-> returning ts_node)
  let ts_node_edit                              = foreign "ts_node_edit" (ptr ts_node @-> ptr ts_input_edit @-> returning void)
  let ts_node_eq                                = foreign "ts_node_eq" (ts_node @-> ts_node @-> returning bool)

  (* TreeCursor *)
  let ts_tree_cursor_new                        = foreign "ts_tree_cursor_new" (ts_node @-> returning ts_tree_cursor)
  let ts_tree_cursor_delete                     = foreign "ts_tree_cursor_delete" (ptr ts_tree_cursor @-> returning void)
  let ts_tree_cursor_reset                      = foreign "ts_tree_cursor_reset" (ptr ts_tree_cursor @-> ts_node @-> returning void)
  let ts_tree_cursor_reset_to                   = foreign "ts_tree_cursor_reset_to" (ptr ts_tree_cursor @-> ptr ts_tree_cursor @-> returning void)
  let ts_tree_cursor_current_node               = foreign "ts_tree_cursor_current_node" (ptr ts_tree_cursor @-> returning ts_node)
  let ts_tree_cursor_current_field_name         = foreign "ts_tree_cursor_current_field_name" (ptr ts_tree_cursor @-> returning string)
  let ts_tree_cursor_current_field_id           = foreign "ts_tree_cursor_current_field_id" (ptr ts_tree_cursor @-> returning uint16_t)
  let ts_tree_cursor_goto_parent                = foreign "ts_tree_cursor_goto_parent" (ptr ts_tree_cursor @-> returning bool)
  let ts_tree_cursor_goto_next_sibling          = foreign "ts_tree_cursor_goto_next_sibling" (ptr ts_tree_cursor @-> returning bool)
  let ts_tree_cursor_goto_previous_sibling      = foreign "ts_tree_cursor_goto_previous_sibling" (ptr ts_tree_cursor @-> returning bool)
  let ts_tree_cursor_goto_first_child           = foreign "ts_tree_cursor_goto_first_child" (ptr ts_tree_cursor @-> returning bool)
  let ts_tree_cursor_goto_last_child            = foreign "ts_tree_cursor_goto_last_child" (ptr ts_tree_cursor @-> returning bool)
  let ts_tree_cursor_goto_descendant            = foreign "ts_tree_cursor_goto_descendant" (ptr ts_tree_cursor @-> uint32_t @-> returning void)
  let ts_tree_cursor_current_descendant_index   = foreign "ts_tree_cursor_current_descendant_index" (ptr ts_tree_cursor @-> returning uint32_t)
  let ts_tree_cursor_current_depth              = foreign "ts_tree_cursor_current_depth" (ptr ts_tree_cursor @-> returning uint32_t)
  let ts_tree_cursor_goto_first_child_for_byte  = foreign "ts_tree_cursor_goto_first_child_for_byte" (ptr ts_tree_cursor @-> uint32_t @-> returning int64_t)
  let ts_tree_cursor_goto_first_child_for_point = foreign "ts_tree_cursor_goto_first_child_for_point" (ptr ts_tree_cursor @-> ts_point @-> returning int64_t)
  let ts_tree_cursor_copy                       = foreign "ts_tree_cursor_copy" (ptr ts_tree_cursor @-> returning ts_tree_cursor)

  (* Query *)
  let ts_query_new                              = foreign "ts_query_new" (ts_language @-> string @-> uint32_t @-> ptr uint32_t @-> ptr uint32_t @-> returning ts_query)
  let ts_query_delete                           = foreign "ts_query_delete" (ts_query @-> returning void)
  let ts_query_pattern_count                    = foreign "ts_query_pattern_count" (ts_query @-> returning uint32_t)
  let ts_query_capture_count                    = foreign "ts_query_capture_count" (ts_query @-> returning uint32_t)
  let ts_query_string_count                     = foreign "ts_query_string_count" (ts_query @-> returning uint32_t)
  let ts_query_start_byte_for_pattern           = foreign "ts_query_start_byte_for_pattern" (ts_query @-> uint32_t @-> returning uint32_t)
  let ts_query_predicates_for_pattern           = foreign "ts_query_predicates_for_pattern" (ts_query @-> uint32_t @-> ptr uint32_t @-> returning (ptr ts_query_predicate_step))
  let ts_query_is_pattern_rooted                = foreign "ts_query_is_pattern_rooted" (ts_query @-> uint32_t @-> returning bool)
  let ts_query_is_pattern_non_local             = foreign "ts_query_is_pattern_non_local" (ts_query @-> uint32_t @-> returning bool)
  let ts_query_is_pattern_guaranteed_at_step    = foreign "ts_query_is_pattern_guaranteed_at_step" (ts_query @-> uint32_t @-> returning bool)
  let ts_query_capture_name_for_id              = foreign "ts_query_capture_name_for_id" (ts_query @-> uint32_t @-> ptr uint32_t @-> returning string)
  let ts_query_capture_quantifier_for_id        = foreign "ts_query_capture_quantifier_for_id" (ts_query @-> uint32_t @-> uint32_t @-> returning uint32_t)
  let ts_query_string_value_for_id              = foreign "ts_query_string_value_for_id" (ts_query @-> uint32_t @-> ptr uint32_t @-> returning string)
  let ts_query_disable_capture                  = foreign "ts_query_disable_capture" (ts_query @-> string @-> uint32_t @-> returning void)
  let ts_query_disable_pattern                  = foreign "ts_query_disable_pattern" (ts_query @-> uint32_t @-> returning void)
  let ts_query_cursor_new                       = foreign "ts_query_cursor_new" (void @-> returning ts_query_cursor)
  let ts_query_cursor_delete                    = foreign "ts_query_cursor_delete" (ts_query_cursor @-> returning void)
  let ts_query_cursor_exec                      = foreign "ts_query_cursor_exec" (ts_query_cursor @-> ts_query @-> ts_node @-> returning void)
  let ts_query_cursor_did_exceed_match_limit    = foreign "ts_query_cursor_did_exceed_match_limit" (ts_query_cursor @-> returning bool)
  let ts_query_cursor_match_limit               = foreign "ts_query_cursor_match_limit" (ts_query_cursor @-> returning uint32_t)
  let ts_query_cursor_set_match_limit           = foreign "ts_query_cursor_set_match_limit" (ts_query_cursor @-> uint32_t @-> returning void)
  let ts_query_cursor_set_byte_range            = foreign "ts_query_cursor_set_byte_range" (ts_query_cursor @-> uint32_t @-> uint32_t @-> returning void)
  let ts_query_cursor_set_point_range           = foreign "ts_query_cursor_set_point_range" (ts_query_cursor @-> ts_point @-> ts_point @-> returning void)
  let ts_query_cursor_next_match                = foreign "ts_query_cursor_next_match" (ts_query_cursor @-> ptr ts_query_match @-> returning bool)
  let ts_query_cursor_remove_match              = foreign "ts_query_cursor_remove_match" (ts_query_cursor @-> uint32_t @-> returning void)
  let ts_query_cursor_next_capture              = foreign "ts_query_cursor_next_capture" (ts_query_cursor @-> ptr ts_query_match @-> ptr uint32_t @-> returning bool)
  let ts_query_cursor_set_max_start_depth       = foreign "ts_query_cursor_set_max_start_depth" (ts_query_cursor @-> uint32_t @-> returning void)

  (* Language *)
  let ts_language_copy                          = foreign "ts_language_copy" (ts_language @-> returning ts_language)
  let ts_language_delete                        = foreign "ts_language_delete" (ts_language @-> returning void)
  let ts_language_symbol_count                  = foreign "ts_language_symbol_count" (ts_language @-> returning uint32_t)
  let ts_language_state_count                   = foreign "ts_language_state_count" (ts_language @-> returning uint32_t)
  let ts_language_symbol_name                   = foreign "ts_language_symbol_name" (ts_language @-> uint16_t @-> returning string)
  let ts_language_symbol_for_name               = foreign "ts_language_symbol_for_name" (ts_language @-> string @-> uint32_t @-> bool @-> returning uint16_t)
  let ts_language_field_count                   = foreign "ts_language_field_count" (ts_language @-> returning uint32_t)
  let ts_language_field_name_for_id             = foreign "ts_language_field_name_for_id" (ts_language @-> uint16_t @-> returning string)
  let ts_language_field_id_for_name             = foreign "ts_language_field_id_for_name" (ts_language @-> string @-> uint32_t @-> returning uint16_t)
  let ts_language_symbol_type                   = foreign "ts_language_symbol_type" (ts_language @-> uint16_t @-> returning uint32_t)
  let ts_language_version                       = foreign "ts_language_version" (ts_language @-> returning uint32_t)
  let ts_language_next_state                    = foreign "ts_language_next_state" (ts_language @-> uint16_t @-> uint16_t @-> returning uint16_t)

  (* Lookahead Iterator *)
  let ts_lookahead_iterator_new                 = foreign "ts_lookahead_iterator_new" (ts_language @-> uint16_t @-> returning ts_lookahead_iterator)
  let ts_lookahead_iterator_delete              = foreign "ts_lookahead_iterator_delete" (ts_language @-> returning void)
  let ts_lookahead_iterator_reset_state         = foreign "ts_lookahead_iterator_reset_state" (ts_lookahead_iterator @-> uint16_t @-> returning bool)
  let ts_lookahead_iterator_reset               = foreign "ts_lookahead_iterator_reset" (ts_lookahead_iterator @-> ts_language @-> uint16_t @-> returning bool)
  let ts_lookahead_iterator_language            = foreign "ts_lookahead_iterator_language" (ts_lookahead_iterator @-> returning ts_language)
  let ts_lookahead_iterator_next                = foreign "ts_lookahead_iterator_next" (ts_lookahead_iterator @-> returning bool)
  let ts_lookahead_iterator_current_symbol      = foreign "ts_lookahead_iterator_current_symbol" (ts_lookahead_iterator @-> returning uint16_t)
  let ts_lookahead_iterator_current_symbol_name = foreign "ts_lookahead_iterator_current_symbol_name" (ts_lookahead_iterator @-> returning string)

  (* Supported Grammars *)
  let tree_sitter_javascript                    = foreign "tree_sitter_javascript" (void @-> returning ts_language)
  let tree_sitter_ocaml                         = foreign "tree_sitter_ocaml" (void @-> returning ts_language)
  let tree_sitter_ocaml_interface               = foreign "tree_sitter_ocaml_interface" (void @-> returning ts_language)
  let tree_sitter_ocaml_type                    = foreign "tree_sitter_ocaml_type" (void @-> returning ts_language)
end
[@@ocamlformat "disable"]
