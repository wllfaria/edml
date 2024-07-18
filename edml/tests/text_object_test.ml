open Core
open Edml
open Text_object
open Types

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
