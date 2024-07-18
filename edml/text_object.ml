open Core
open Assert
open Types

type action =
  | TypeChar of char
  | DeleteLine
[@@deriving eq, show { with_path = false }]

type t =
  { content : string list
  ; lines : int
  }
[@@deriving eq, show { with_path = false }]

let make source =
  let content = String.split_lines source in
  let lines = List.length content in
  { content; lines }
;;

let insert_char ~char ~text_object ~anchor =
  assert_true
    (text_object.lines > anchor.row)
    (Format.sprintf "editing out of bounds on line: %d" anchor.row);
  let line = List.nth_exn text_object.content anchor.row in
  assert_true
    (String.length line > anchor.col)
    (Format.sprintf "editing out of bounds on line %d column %d" anchor.row anchor.col);
  let new_line =
    let line_len = String.length line in
    let prefix = String.sub line ~pos:0 ~len:anchor.col in
    let suffix = String.sub line ~pos:anchor.col ~len:(line_len - anchor.col) in
    prefix ^ Char.to_string char ^ suffix
  in
  let content =
    let before = List.take text_object.content anchor.row in
    let after = List.drop text_object.content (anchor.row + 1) in
    before @ [ new_line ] @ after
  in
  { text_object with content }
;;

let handle_action ~text_object ~action ~anchor =
  match action with
  | TypeChar char -> insert_char ~char ~text_object ~anchor
  | DeleteLine -> text_object
;;

let%test _ =
  let expected =
    { content = [ "#include <stdio.h>"; ""; "int main(void) {"; "    return 0;"; "}" ]
    ; lines = 5
    }
  in
  let result = make {|#include <stdio.h>

int main(void) {
    return 0;
}|} in
  [%eq: t] result expected
;;

let%test "updates correct spot on text object" =
  let expected =
    { content =
        [ "#include <stdio.h>"
        ; ""
        ; "int main(int argc, char *argv[]) {"
        ; "    return 0;"
        ; "}"
        ]
    ; lines = 5
    }
  in
  let text_object = ref @@ make "#include <stdio.h>\n\nint main() {\n    return 0;\n}" in
  let input_text = "int argc, char *argv[]" in
  let actions = String.fold input_text ~init:[] ~f:(fun acc c -> TypeChar c :: acc) in
  let anchor = ref { row = 2; col = 9 } in
  List.iter actions ~f:(fun action ->
    text_object := handle_action ~text_object:!text_object ~action ~anchor:!anchor);
  anchor := { !anchor with col = !anchor.col + 1 };
  [%eq: t] expected !text_object
;;
