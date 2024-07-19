open Ctypes

(* module Enums (F : Cstubs.Types.TYPE) = struct *)
(*   open F *)
(**)
(*   let ts_input_encoding_utf8 = constant "TSInputEncodingUTF8" int64_t *)
(*   let ts_input_encoding_utf16 = constant "TSInputEncodingUTF16" int64_t *)
(**)
(*   (* TSInput *) *)
(*   type ts_input_encoding = *)
(*     | UTF8 *)
(*     | UTF16 *)
(**)
(*   let ts_input_encoding = *)
(*     enum *)
(*       "TSInputEncoding" *)
(*       [ UTF8, ts_input_encoding_utf8; UTF16, ts_input_encoding_utf16 ] *)
(*   ;; *)
(* end *)

module Types = struct
  type ts_language = unit ptr
  type ts_parser = unit ptr
  type ts_tree = unit ptr
  type ts_range
  type ts_point
  type ts_input

  (* Opaque *)
  let parser : ts_parser typ = ptr void
  let language : ts_language typ = ptr void
  let tree : ts_tree typ = ptr void

  (* TSPoint *)
  let point : ts_point structure typ = structure "TSPoint"
  let row = field point "row" uint32_t
  let column = field point "column" uint32_t
  let () = seal point

  (* TSRange *)
  let range : ts_range structure typ = structure "TSRange"
  let start_point = field range "start_point" point
  let end_point = field range "end_point" point
  let start_byte = field range "start_byte" uint32_t
  let end_byte = field range "end_byte" uint32_t
  let () = seal range

  (* TSInput *)
  let input : ts_input structure typ = structure "TSInput"
  let payload = field input "payload" (ptr void)
  let read_fn = ptr void @-> uint32_t @-> point @-> ptr uint32_t @-> returning (ptr char)
  let read = field input "read" (static_funptr read_fn)
  let encoding = field input "encoding" int
  let () = seal input
end

module Bindings (F : Ctypes.FOREIGN) = struct
  open F
  open Types

  let parser_new = foreign "ts_parser_new" (void @-> returning parser)
  let parser_delete = foreign "ts_parser_delete" (parser @-> returning void)
  let parser_language = foreign "ts_parser_language" (parser @-> returning language)

  let parser_set_language =
    foreign "ts_parser_set_language" (parser @-> language @-> returning bool)
  ;;

  let parser_set_included_ranges =
    foreign
      "ts_parser_set_included_ranges"
      (parser @-> ptr range @-> uint32_t @-> returning bool)
  ;;

  let parser_included_ranges =
    foreign "ts_parser_included_ranges" (parser @-> ptr uint32_t @-> returning (ptr range))
  ;;

  let parser_parse =
    foreign "ts_parser_parse" (parser @-> tree @-> input @-> returning tree)
  ;;

  let parser_parse_string =
    foreign
      "ts_parser_parse_string"
      (parser @-> tree @-> string @-> uint32_t @-> returning tree)
  ;;
end
