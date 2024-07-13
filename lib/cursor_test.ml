open Cursor

let make_text_object () =
  Text_object.make {|#include <stdio.h>

int main(void) {
    return 0;
}|}
;;

let%test "move left with available space" =
  let cursor = { row = 0; col = 10; real_col = 9 } in
  let expect = { row = 0; col = 9; real_col = 9 } in
  let result = move_left cursor in
  [%eq: t] expect result
;;

let%test "move left without available space" =
  let cursor = { row = 1; col = 0; real_col = 0 } in
  let expect = { row = 1; col = 0; real_col = 0 } in
  let result = move_left cursor in
  [%eq: t] expect result
;;

let%test "move down to line with same or bigger size" =
  let cursor = { row = 0; col = 0; real_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 1; col = 0; real_col = 0 } in
  let result = move_down cursor text_object in
  [%eq: t] expect result
;;

let%test "move down to line with smaller size" =
  let cursor = { row = 0; col = 10; real_col = 10 } in
  let text_object = make_text_object () in
  let expect = { row = 1; col = 0; real_col = 10 } in
  let result = move_down cursor text_object in
  [%eq: t] expect result
;;

let%test "move down to line with smaller size, then to line with bigger size" =
  let cursor = { row = 0; col = 10; real_col = 10 } in
  let text_object = make_text_object () in
  let expect = { row = 1; col = 0; real_col = 10 } in
  let result = move_down cursor text_object in
  match [%eq: t] expect result with
  | false -> false
  | true ->
    let cursor = expect in
    let expect = { row = 2; col = 10; real_col = 10 } in
    let result = move_down cursor text_object in
    [%eq: t] expect result
;;

let%test "move down to line with smaller size, then to line with enough size" =
  let cursor = { row = 0; col = 17; real_col = 17 } in
  let text_object = make_text_object () in
  let expect = { row = 1; col = 0; real_col = 17 } in
  let result = move_down cursor text_object in
  match [%eq: t] expect result with
  | false -> false
  | true ->
    let cursor = expect in
    let expect = { row = 2; col = 16; real_col = 17 } in
    let result = move_down cursor text_object in
    [%eq: t] expect result
;;

let%test "move down without lines below" =
  let cursor = { row = 4; col = 0; real_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 4; col = 0; real_col = 0 } in
  let result = move_down cursor text_object in
  [%eq: t] expect result
;;

let%test "move up with lines above" =
  let cursor = { row = 1; col = 0; real_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 0; col = 0; real_col = 0 } in
  let result = move_up cursor text_object in
  [%eq: t] expect result
;;

let%test "move up without lines above" =
  let cursor = { row = 0; col = 0; real_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 0; col = 0; real_col = 0 } in
  let result = move_up cursor text_object in
  [%eq: t] expect result
;;

let%test "move up to smaller line above" =
  let cursor = { row = 2; col = 10; real_col = 10 } in
  let text_object = make_text_object () in
  let expect = { row = 1; col = 0; real_col = 10 } in
  let result = move_up cursor text_object in
  [%eq: t] expect result
;;

let%test "move up to bigger line above" =
  let cursor = { row = 1; col = 0; real_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 0; col = 0; real_col = 0 } in
  let result = move_up cursor text_object in
  [%eq: t] expect result
;;

let%test "move right with available space" =
  let cursor = { row = 0; col = 0; real_col = 0 } in
  let text_object = make_text_object () in
  let expect = { row = 0; col = 1; real_col = 1 } in
  let result = move_right cursor text_object in
  [%eq: t] expect result
;;

let%test "move right at the end of the line" =
  let cursor = { row = 0; col = 18; real_col = 18 } in
  let text_object = make_text_object () in
  let expect = { row = 0; col = 18; real_col = 18 } in
  let result = move_right cursor text_object in
  [%eq: t] expect result
;;
