type action = Cursor of Cursor.cursor_action

let handle_key_event key_event =
  let open Ansi.Event in
  match key_event with
  | { code = Left; _ } -> Some (Cursor Cursor.MoveLeft)
  | { code = Down; _ } -> Some (Cursor Cursor.MoveDown)
  | { code = Up; _ } -> Some (Cursor Cursor.MoveUp)
  | { code = Right; _ } -> Some (Cursor Cursor.MoveRight)
  | _ -> None
;;
