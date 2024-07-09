type text_object =
  { content : string list
  ; lines : int
  }
[@@deriving eq, show]

let make_text_object source =
  let content = String.split_on_char '\n' source in
  let lines = List.length content in
  { content; lines }
;;

let%test _ =
  let expected =
    { content = [ "#include <stdio.h>"; ""; "int main(void) {"; "    return 0;"; "}" ]
    ; lines = 5
    }
  in
  let result =
    make_text_object {|#include <stdio.h>

int main(void) {
    return 0;
}|}
  in
  [%eq: text_object] result expected
;;
