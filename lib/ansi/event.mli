type keycode =
  | Char of char
  | Tab
  | Enter
  | Backspace

type key_modifier =
  | Control
  | Shift
  | Normal

type key_event =
  { code : keycode
  ; modifier : key_modifier
  }

type event = KeyEvent of key_event

val show_event : event -> string
val read : unit -> event
