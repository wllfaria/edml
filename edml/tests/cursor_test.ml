open Core
open Edml
open Cursor

let make_text_object () =
  Text_object.make {|#include <stdio.h>

int main(void) {
    return 0;
}|}
;;

let%test "move left with available space" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 20; cols = 20 } in
  let text_object = make_text_object () in
  let cursor = { row = 0; col = 10; real_col = 9; offset_row = 0; offset_col = 0 } in
  let expect = { row = 0; col = 9; real_col = 9; offset_row = 0; offset_col = 0 } in
  let result = move_left cursor text_object dimensions in
  [%eq: cursor] expect result
;;

let%test "move left without available space" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 20; cols = 20 } in
  let text_object = make_text_object () in
  let cursor = { row = 1; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let expect = { row = 1; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let result = move_left cursor text_object dimensions in
  [%eq: cursor] expect result
;;

let%test "move down to line with same or bigger size" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 20; cols = 20 } in
  let cursor = { row = 0; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 1; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let result = move_down cursor text_object dimensions in
  [%eq: cursor] expect result
;;

let%test "move down to line with smaller size" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 20; cols = 20 } in
  let cursor = { row = 0; col = 10; real_col = 10; offset_row = 0; offset_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 1; col = 0; real_col = 10; offset_row = 0; offset_col = 0 } in
  let result = move_down cursor text_object dimensions in
  [%eq: cursor] expect result
;;

let%test "move down to line with smaller size, then to line with bigger size" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 20; cols = 20 } in
  let cursor = { row = 0; col = 10; real_col = 10; offset_row = 0; offset_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 1; col = 0; real_col = 10; offset_row = 0; offset_col = 0 } in
  let result = move_down cursor text_object dimensions in
  match [%eq: cursor] expect result with
  | false -> false
  | true ->
    let cursor = expect in
    let expect = { row = 2; col = 10; real_col = 10; offset_row = 0; offset_col = 0 } in
    let result = move_down cursor text_object dimensions in
    [%eq: cursor] expect result
;;

let%test "move down to line with smaller size, then to line with enough size" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 20; cols = 20 } in
  let cursor = { row = 0; col = 17; real_col = 17; offset_row = 0; offset_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 1; col = 0; real_col = 17; offset_row = 0; offset_col = 0 } in
  let result = move_down cursor text_object dimensions in
  match [%eq: cursor] expect result with
  | false -> false
  | true ->
    let cursor = expect in
    let expect = { row = 2; col = 16; real_col = 17; offset_row = 0; offset_col = 0 } in
    let result = move_down cursor text_object dimensions in
    [%eq: cursor] expect result
;;

let%test "move down without lines below" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 20; cols = 20 } in
  let cursor = { row = 4; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 4; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let result = move_down cursor text_object dimensions in
  [%eq: cursor] expect result
;;

let%test "move up with lines above" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 20; cols = 20 } in
  let cursor = { row = 1; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 0; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let result = move_up cursor text_object dimensions in
  [%eq: cursor] expect result
;;

let%test "move up without lines above" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 20; cols = 20 } in
  let cursor = { row = 0; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 0; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let result = move_up cursor text_object dimensions in
  [%eq: cursor] expect result
;;

let%test "move up to smaller line above" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 20; cols = 20 } in
  let cursor = { row = 2; col = 10; real_col = 10; offset_row = 0; offset_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 1; col = 0; real_col = 10; offset_row = 0; offset_col = 0 } in
  let result = move_up cursor text_object dimensions in
  [%eq: cursor] expect result
;;

let%test "move up to bigger line above" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 20; cols = 20 } in
  let cursor = { row = 1; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 0; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let result = move_up cursor text_object dimensions in
  [%eq: cursor] expect result
;;

let%test "move right with available space" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 20; cols = 20 } in
  let cursor = { row = 0; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 0; col = 1; real_col = 1; offset_row = 0; offset_col = 0 } in
  let result = move_right cursor text_object dimensions in
  [%eq: cursor] expect result
;;

let%test "move right at the end of the line" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 20; cols = 20 } in
  let cursor = { row = 0; col = 18; real_col = 18; offset_row = 0; offset_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 0; col = 18; real_col = 18; offset_row = 0; offset_col = 0 } in
  let result = move_right cursor text_object dimensions in
  [%eq: cursor] expect result
;;

let%test "scrolling down works as expected" =
  let dimensions : Ansi.Terminal.dimensions = { rows = 40; cols = 20 } in
  let cursor = { row = 0; col = 0; real_col = 0; offset_row = 0; offset_col = 0 } in
  let content = String.make 71 '\n' in
  let text_object = Text_object.make content in
  let expect = { row = 70; col = 0; real_col = 0; offset_row = 31; offset_col = 0 } in
  let result = move_to_bottom cursor text_object dimensions in
  [%eq: cursor] expect result
;;
