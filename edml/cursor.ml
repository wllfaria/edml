open Core

type t =
  { row : int
  ; col : int
  ; real_col : int
  ; offset_row : int
  ; offset_col : int
  }
[@@deriving eq, show { with_path = false }]

type action =
  | MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown
  | MoveToBottom
  | MoveToTop
[@@deriving eq, show { with_path = false }]

let make () = { col = 0; row = 0; real_col = 0; offset_col = 0; offset_row = 0 }

let adjust_column cursor _line vp_dimensions =
  let open Ansi.Terminal in
  let current_col = cursor.col in
  let vp_width = vp_dimensions.cols in
  if current_col - cursor.offset_col >= vp_width
  then { cursor with offset_col = current_col - vp_width + 1 }
  else if current_col - cursor.offset_col <= 0
  then { cursor with offset_col = cursor.offset_col - (cursor.offset_col - current_col) }
  else cursor
;;

(* let line_len = String.length line in *)
(* if cursor.real_col < line_len *)
(* then { cursor with col = cursor.real_col } *)
(* else { cursor with col = max line_len 0 } *)

let adjust_row cursor vp_dimensions =
  let open Ansi.Terminal in
  let current_row = cursor.row in
  let vp_height = vp_dimensions.rows in
  if current_row - cursor.offset_row >= vp_height
  then { cursor with offset_row = current_row - vp_height + 1 }
  else if current_row - cursor.offset_row <= 0
  then { cursor with offset_row = cursor.offset_row - (cursor.offset_row - current_row) }
  else cursor
;;

let move_left cursor text_object vp_dimensions =
  let open Text_object in
  let cursor = if cursor.col > 0 then { cursor with col = cursor.col - 1 } else cursor in
  let line = List.nth_exn text_object.content cursor.row in
  let cursor = adjust_column cursor line vp_dimensions in
  { cursor with real_col = cursor.col }
;;

let move_right cursor text_object vp_dimensions =
  let open Text_object in
  let line = List.nth_exn text_object.content cursor.row in
  let cursor =
    if cursor.col < String.length line
    then { cursor with col = cursor.col + 1 }
    else cursor
  in
  let cursor = adjust_column cursor line vp_dimensions in
  { cursor with real_col = cursor.col }
;;

let move_up cursor text_object vp_dimensions =
  let open Text_object in
  if cursor.row > 0
  then (
    let cursor = { cursor with row = cursor.row - 1 } in
    let line = List.nth_exn text_object.content cursor.row in
    let cursor = adjust_row cursor vp_dimensions in
    adjust_column cursor line vp_dimensions)
  else cursor
;;

let move_down cursor text_object vp_dimensions =
  let open Text_object in
  let cursor =
    if cursor.row < List.length text_object.content - 1
    then { cursor with row = cursor.row + 1 }
    else cursor
  in
  let line = List.nth_exn text_object.content cursor.row in
  let cursor = adjust_row cursor vp_dimensions in
  adjust_column cursor line vp_dimensions
;;

let move_to_top cursor text_object vp_dimensions =
  let open Text_object in
  let cursor = { cursor with row = 0 } in
  let line = List.nth_exn text_object.content cursor.row in
  let cursor = adjust_row cursor vp_dimensions in
  adjust_column cursor line vp_dimensions
;;

let move_to_bottom cursor text_object vp_dimensions =
  let open Text_object in
  let cursor = { cursor with row = text_object.lines - 1 } in
  let line = List.nth_exn text_object.content cursor.row in
  let cursor = adjust_row cursor vp_dimensions in
  adjust_column cursor line vp_dimensions
;;

let handle_action action cursor text_object vp_dimensions =
  match action with
  | MoveLeft -> move_left cursor text_object vp_dimensions
  | MoveRight -> move_right cursor text_object vp_dimensions
  | MoveUp -> move_up cursor text_object vp_dimensions
  | MoveDown -> move_down cursor text_object vp_dimensions
  | MoveToTop -> move_to_top cursor text_object vp_dimensions
  | MoveToBottom -> move_to_bottom cursor text_object vp_dimensions
;;
