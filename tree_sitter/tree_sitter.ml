type ts_language
type ts_parser
type ts_tree
type ts_query
type ts_query_cursor
type ts_lookahead_iterator

type ts_point =
  { row : int
  ; column : int
  }

type ts_range =
  { start_point : ts_point
  ; end_point : ts_point
  ; start_byte : int
  ; end_byte : int
  }

external ts_parser_new : unit -> ts_parser = "caml_ts_parser_new"
external ts_parser_delete : ts_parser -> unit = "caml_ts_parser_delete"
external ts_parser_language : ts_parser -> ts_language = "caml_ts_parser_language"

external ts_parser_set_language
  :  ts_parser
  -> ts_language
  -> bool
  = "caml_ts_parser_set_language"

external ts_parser_set_included_ranges
  :  ts_parser
  -> ts_range
  -> int
  -> bool
  = "caml_ts_parser_set_included_ranges"
