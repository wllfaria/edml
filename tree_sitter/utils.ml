open Types
open Bindings

let size_t_of_int int = Unsigned.Size_t.of_int int
let uint64_to_int uint = Unsigned.UInt64.to_int uint
let uint64_of_int int = Unsigned.UInt64.of_int int
let uint32_to_int uint = Unsigned.UInt32.to_int uint
let uint32_of_int int = Unsigned.UInt32.of_int int
let uint16_of_int int = Unsigned.UInt16.of_int int

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
