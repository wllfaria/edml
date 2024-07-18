open Edml
open Editor

type action =
  | CursorAction of Cursor.action
  | TextObjectAction of Text_object.action
  | ChangeMode of mode
[@@deriving eq, show { with_path = false }]
