open Core

type t =
  { content : string list
  ; lines : int
  }
[@@deriving eq, show]

let make source =
  let content = String.split_lines source in
  let lines = List.length content in
  { content; lines }
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
