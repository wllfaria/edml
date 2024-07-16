open Core

type t =
  { row : int
  ; col : int
  ; real_col : int
  }
[@@deriving eq, show { with_path = false }]

type action =
  | MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
[@@deriving show { with_path = false }]

let min a b = if a > b then b else a
let mac a b = if a < b then b else a
let make () = { col = 0; row = 0; real_col = 0 }

let adjust_column cursor line =
  let line_len = String.length line in
  if cursor.real_col < line_len
  then { cursor with col = cursor.real_col }
  else { cursor with col = max line_len 0 }
;;

let move_left cursor =
  let cursor = if cursor.col > 0 then { cursor with col = cursor.col - 1 } else cursor in
  { cursor with real_col = cursor.col }
;;

let move_right cursor text_object =
  let open Text_object in
  let line = List.nth_exn text_object.content cursor.row in
  let cursor =
    if cursor.col < String.length line
    then { cursor with col = cursor.col + 1 }
    else cursor
  in
  { cursor with real_col = cursor.col }
;;

let move_up cursor text_object =
  let open Text_object in
  if cursor.row > 0
  then (
    let cursor = { cursor with row = cursor.row - 1 } in
    let line = List.nth_exn text_object.content cursor.row in
    adjust_column cursor line)
  else cursor
;;

let move_down cursor text_object =
  let open Text_object in
  let cursor =
    if cursor.row < List.length text_object.content - 1
    then { cursor with row = cursor.row + 1 }
    else cursor
  in
  let line = List.nth_exn text_object.content cursor.row in
  adjust_column cursor line
;;

let handle_action action cursor text_object =
  match action with
  | MoveLeft -> move_left cursor
  | MoveRight -> move_right cursor text_object
  | MoveUp -> move_up cursor text_object
  | MoveDown -> move_down cursor text_object
;;
