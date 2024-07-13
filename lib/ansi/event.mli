type event = 
  | FocusGained
  | FocusLost
  | KeyEvent of key_event

and keycode =
  | Char of char
  | Tab
  | Enter
  | Backspace
  | Left
  | Right
  | Up
  | Down
  | Esc
  | Home
  | End
  | BackTab

and key_modifier =
  | Control
  | Shift
  | Normal

and key_event =
  { code : keycode
  ; modifier : key_modifier
  }

val show_event : event -> string
val show_key_event : key_event -> string
val read : unit -> event
