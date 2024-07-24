open Core
open Types
open Trie

type action =
  | CursorAction of Cursor.action
  | TextObjectAction of Text_object.action
  | ChangeMode of mode

type event =
  | FocusGained
  | FocusLost
  | KeyEvent of key_event
[@@deriving eq, show { with_path = false }]

and key_event =
  { code : keycode
  ; modifier : key_modifier
  }
[@@deriving eq, show { with_path = false }]

and keycode =
  | Char of char
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
  | F of int
[@@deriving eq, show { with_path = false }]

and key_modifier =
  | Control
  | Shift
  | Normal
[@@deriving eq, show { with_path = false }]

type state =
  | Initial
  | Escape
  | Csi
[@@deriving show { with_path = false }]

let buffered_keymap = ref None
let buffered_action = ref None

let normal_mode_mappings =
  let open Cursor in
  let open Text_object in
  let mappings =
    [ "h", [ CursorAction MoveLeft ]
    ; "j", [ CursorAction MoveDown ]
    ; "k", [ CursorAction MoveUp ]
    ; "l", [ CursorAction MoveRight ]
    ; "G", [ CursorAction MoveToBottom ]
    ; "gg", [ CursorAction MoveToTop ]
    ; "0", [ CursorAction MoveToLineStart ]
    ; "$", [ CursorAction MoveToLineEnd ]
    ; "dd", [ TextObjectAction DeleteLine ]
    ; "D", [ TextObjectAction DeleteUntilEOL ]
    ; "x", [ TextObjectAction DeleteCurrChar ]
    ; "X", [ TextObjectAction DeletePrevChar; CursorAction MoveLeft ]
    ]
  in
  let trie = Trie.empty () in
  List.fold mappings ~init:trie ~f:(fun node (keymap, action) ->
    Trie.add_word ~node ~word:keymap ~data:(Some action))
;;

let string_of_key_event key_event =
  match key_event with
  | { code = Char c; modifier = Normal } -> Char.to_string c
  | { code = Char c; modifier = Control } -> "<c-" ^ Char.to_string c ^ ">"
  | { code = Char c; modifier = Shift } -> Char.to_string c
  | { code = Enter; _ } -> "<cr>"
  | { code = Backspace; _ } -> "<bs>"
  | { code = Left; _ } -> "left"
  | { code = Right; _ } -> "right"
  | { code = Up; _ } -> "up"
  | { code = Down; _ } -> "down"
  | { code = Esc; _ } -> "<esc>"
  | { code = Home; _ } -> "<home>"
  | { code = End; _ } -> "<end>"
  | { code = BackTab; _ } -> "<bt>"
  | { code = F n; _ } -> "f" ^ string_of_int n
;;

let from_normal_mode key_event =
  let as_string = string_of_key_event key_event in
  let full_keymap =
    match !buffered_keymap with
    | Some key -> key ^ as_string
    | None -> as_string
  in
  let maybe_action = Trie.find_word ~node:normal_mode_mappings ~word:full_keymap in
  match maybe_action with
  | Some result ->
    (match result.data with
     | Some action when phys_same result.continues true ->
       buffered_action := Some action;
       buffered_keymap := Some full_keymap;
       (* we should start a timer here *)
       None
     | Some action ->
       buffered_action := None;
       buffered_keymap := None;
       Some action
     | None when phys_same result.continues true ->
       buffered_keymap := Some full_keymap;
       None
     | None ->
       buffered_keymap := None;
       buffered_action := None;
       None)
  | None -> None
;;

let from_insert_mode _key_event = None

let handle_key_event key_event (mode : mode) =
  match mode with
  | Normal -> from_normal_mode key_event
  | Insert -> from_insert_mode key_event
;;
