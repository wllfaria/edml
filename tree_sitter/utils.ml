open Bindings
module TS = Bindings (Tree_sitter_generated)
open Tree_sitter_types
open Core

let size_t_of_int int = Unsigned.Size_t.of_int int
let uint64_to_int uint = Unsigned.UInt64.to_int uint
let uint64_of_int int = Unsigned.UInt64.of_int int
let uint32_to_int uint = Unsigned.UInt32.to_int uint
let uint32_of_int int = Unsigned.UInt32.of_int int
let uint16_of_int int = Unsigned.UInt16.of_int int
let uint16_to_int int = Unsigned.UInt16.to_int int

let ts_point_of_point point =
  let open Ctypes in
  let p = make Types.ts_point in
  setf p Types.ts_point_row @@ uint32_of_int point.row;
  setf p Types.ts_point_column @@ uint32_of_int point.col;
  p
;;

let ts_point_to_point ts_point =
  let open Ctypes in
  let row = uint32_to_int @@ getf ts_point Types.ts_point_row in
  let col = uint32_to_int @@ getf ts_point Types.ts_point_column in
  { row; col }
;;

let ts_range_of_range range =
  let open Ctypes in
  let r = make Types.ts_range in
  setf r Types.start_byte @@ uint32_of_int range.start_byte;
  setf r Types.end_byte @@ uint32_of_int range.end_byte;
  setf r Types.start_point @@ ts_point_of_point range.start_point;
  setf r Types.end_point @@ ts_point_of_point range.end_point;
  r
;;

let ts_range_to_range ts_range =
  let open Ctypes in
  let start_byte = uint32_to_int @@ getf ts_range Types.start_byte in
  let end_byte = uint32_to_int @@ getf ts_range Types.end_byte in
  let start_point = ts_point_to_point @@ getf ts_range Types.start_point in
  let end_point = ts_point_to_point @@ getf ts_range Types.end_point in
  { start_byte; end_byte; start_point; end_point }
;;

let ts_input_encoding_of_encoding encoding =
  match encoding with
  | InputEncodingUTF8 -> 0
  | InputEncodingUTF16 -> 1
;;

let ts_input_of_input input =
  let open Ctypes in
  let i = make Types.ts_input in
  let payload = allocate_n (ptr void) ~count:1 in
  payload <-@ input.payload;
  let read_fn payload_ptr byte_index position bytes_read =
    let payload = from_voidp void payload_ptr in
    let offset = uint32_to_int byte_index in
    let point = ts_point_to_point position in
    let bytes_read = uint32_to_int !@bytes_read in
    let result = input.read payload offset point bytes_read in
    let char_ptr = allocate_n char ~count:1 in
    char_ptr <-@ result;
    char_ptr
  in
  let read_fn =
    coerce (Foreign.funptr Types.read_fn) (static_funptr Types.read_fn) read_fn
  in
  let encoding = ts_input_encoding_of_encoding input.encoding in
  setf i Types.ts_input_payload @@ to_voidp payload;
  setf i Types.ts_input_encoding @@ encoding;
  setf i Types.ts_input_read @@ read_fn;
  i
;;

let ts_node_to_node ts_node =
  let start_byte = uint32_to_int @@ TS.ts_node_start_byte ts_node in
  let end_byte = uint32_to_int @@ TS.ts_node_end_byte ts_node in
  let start_point = ts_point_to_point @@ TS.ts_node_start_point ts_node in
  let end_point = ts_point_to_point @@ TS.ts_node_end_point ts_node in
  let range = { start_byte; end_byte; start_point; end_point } in
  { inner = ts_node; range }
;;

let ts_query_capture_to_query_capture ts_capture =
  let open Ctypes in
  let node = getf ts_capture Types.ts_query_capture_node in
  let node = ts_node_to_node node in
  let index = uint32_to_int @@ getf ts_capture Types.ts_query_capture_index in
  { node; index }
;;

let ts_query_match_to_query_match ts_match =
  let open Ctypes in
  let captures_ptr = getf ts_match Types.ts_query_match_capture_captures in
  let capture_count = uint16_to_int @@ getf ts_match Types.ts_query_match_capture_count in
  let pattern_index = uint16_to_int @@ getf ts_match Types.ts_query_match_pattern_index in
  let id = uint32_to_int @@ getf ts_match Types.ts_query_match_id in
  let captures = CArray.to_list @@ CArray.from_ptr captures_ptr capture_count in
  let captures = List.map captures ~f:ts_query_capture_to_query_capture in
  { id; pattern_index; capture_count; captures }
;;

let get_query_capture_names ts_query idx max =
  let capture_names = Array.create "" ~len:max in
  let open Ctypes in
  let rec loop idx max =
    match idx < max with
    | true ->
      let uidx = uint32_of_int idx in
      let len = allocate_n uint32_t ~count:1 in
      let result = TS.ts_query_capture_name_for_id ts_query uidx len in
      capture_names.(idx) <- result;
      loop (idx + 1) max
    | false -> ()
  in
  loop idx max;
  capture_names
;;

let get_query_capture_quantifiers ts_query pattern_idx pattern_count capture_count =
  let capture_quantifiers =
    Array.create ~len:pattern_count @@ Array.create 0 ~len:capture_count
  in
  let rec loop pattern_idx max =
    match pattern_idx < max with
    | true ->
      let pattern_quantifiers = Array.create 0 ~len:max in
      get_pattern_capture_quantifiers 0 capture_count pattern_quantifiers;
      loop (pattern_idx + 1) max
    | false -> ()
  and get_pattern_capture_quantifiers capture_idx max acc =
    match capture_idx < max with
    | true ->
      let capture_uidx = uint32_of_int capture_idx in
      let pattern_uidx = uint32_of_int pattern_idx in
      let quantifier =
        uint32_to_int
        @@ TS.ts_query_capture_quantifier_for_id ts_query pattern_uidx capture_uidx
      in
      acc.(capture_idx) <- quantifier;
      get_pattern_capture_quantifiers (capture_idx + 1) max acc
    | false -> ()
  in
  loop 0 pattern_count;
  capture_quantifiers
;;

let ts_query_to_query ts_query =
  let capture_count = uint32_to_int @@ TS.ts_query_capture_count ts_query in
  let pattern_count = uint32_to_int @@ TS.ts_query_pattern_count ts_query in
  let capture_names = get_query_capture_names ts_query 0 capture_count in
  let capture_quantifiers =
    get_query_capture_quantifiers ts_query 0 pattern_count capture_count
  in
  { inner = ts_query; capture_names; capture_quantifiers }
;;
