type t =
  { row : int
  ; col : int
  }
[@@deriving show { with_path = false }]

type cursor_action =
  | MoveLeft
  | MoveRight
  | MoveUp
  | MoveDown

let make () = { col = 0; row = 0 }

let move_left cursor =
  match cursor.col with
  | col when col == 0 && cursor.row > 0 -> { cursor with row = cursor.row - 1 }
  | _ -> { cursor with col = cursor.col - 1 }
;;

let move_right cursor = { cursor with col = cursor.col + 1 }

let move_up cursor =
  if cursor.row > 0 then { cursor with row = cursor.row - 1 } else cursor
;;

let move_down cursor = { cursor with row = cursor.row + 1 }

let handle_action cursor action =
  match action with
  | MoveLeft -> move_left cursor
  | MoveRight -> move_right cursor
  | MoveUp -> move_up cursor
  | MoveDown -> move_down cursor
;;
