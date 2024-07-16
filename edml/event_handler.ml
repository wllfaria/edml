open Ansi.Event
open Editor

type action =
  | Cursor of Cursor.action
  | TextObject of Text_object.action
  | ChangeMode of mode

let normal_mode_key_event key_event =
  match key_event with
  | { code = Char 'h'; _ } -> Some (Cursor Cursor.MoveLeft)
  | { code = Char 'j'; _ } -> Some (Cursor Cursor.MoveDown)
  | { code = Char 'k'; _ } -> Some (Cursor Cursor.MoveUp)
  | { code = Char 'l'; _ } -> Some (Cursor Cursor.MoveRight)
  | { code = Char 'i'; _ } -> Some (ChangeMode Insert)
  | _ -> None
;;

let insert_mode_key_event key_event =
  match key_event with
  | { code = Left; _ } -> Some (Cursor Cursor.MoveLeft)
  | { code = Down; _ } -> Some (Cursor Cursor.MoveDown)
  | { code = Up; _ } -> Some (Cursor Cursor.MoveUp)
  | { code = Right; _ } -> Some (Cursor Cursor.MoveRight)
  | { code = Esc; _ } -> Some (ChangeMode Normal)
  | { code = Char c; _ } -> Some (TextObject (Text_object.TypeChar c))
  | _ -> None
;;

let handle_key_event key_event mode =
  match mode with
  | Normal -> normal_mode_key_event key_event
  | Insert -> insert_mode_key_event key_event
;;
