open Ansi.Event
open Editor

type action = Cursor of Cursor.cursor_action

let normal_mode_key_event key_event =
  match key_event with
  | { code = Char 'h'; _ } -> Some (Cursor Cursor.MoveLeft)
  | { code = Char 'j'; _ } -> Some (Cursor Cursor.MoveDown)
  | { code = Char 'k'; _ } -> Some (Cursor Cursor.MoveUp)
  | { code = Char 'l'; _ } -> Some (Cursor Cursor.MoveRight)
  | _ -> None
;;

let insert_mode_key_event key_event =
  match key_event with
  | { code = Left; _ } -> Some (Cursor Cursor.MoveLeft)
  | { code = Down; _ } -> Some (Cursor Cursor.MoveDown)
  | { code = Up; _ } -> Some (Cursor Cursor.MoveUp)
  | { code = Right; _ } -> Some (Cursor Cursor.MoveRight)
  | _ -> None
;;

let handle_key_event key_event mode =
  match mode with
  | Normal -> normal_mode_key_event key_event
  | Insert -> insert_mode_key_event key_event
;;
