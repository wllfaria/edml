open Edml

let key_code_of_ansi (keycode : Ansi.Event.keycode) =
  let open Event_handler in
  match keycode with
  | Char c -> Char c
  | Enter -> Enter
  | Backspace -> Backspace
  | Left -> Left
  | Right -> Right
  | Up -> Up
  | Down -> Down
  | Esc -> Esc
  | Home -> Home
  | End -> End
  | BackTab -> BackTab
  | F n -> F n
;;

let modifier_of_ansi (ansi_modifier : Ansi.Event.key_modifier) =
  let open Event_handler in
  match ansi_modifier with
  | Normal -> Normal
  | Control -> Control
  | Shift -> Shift
;;

let key_event_of_ansi (ansi_key_event : Ansi.Event.key_event) =
  let open Event_handler in
  let code = ansi_key_event.code in
  let modifier = ansi_key_event.modifier in
  { code = key_code_of_ansi code; modifier = modifier_of_ansi modifier }
;;

let event_of_ansi (ansi_event : Ansi.Event.event) =
  let open Event_handler in
  match ansi_event with
  | FocusGained -> FocusGained
  | FocusLost -> FocusLost
  | KeyEvent ev -> KeyEvent (key_event_of_ansi ev)
;;
