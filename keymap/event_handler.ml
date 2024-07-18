open Edml
open Editor
open Keymap_provider

let handle_key_event key_event mode =
  match mode with
  | Normal -> from_normal_mode key_event
  | Insert -> from_insert_mode key_event
;;
