open Core

let tty_buffer_size = 10

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
[@@deriving show { with_path = false }]

let initial_process ~buffer ~idx ~len =
  assert (!idx < len);
  match Bytes.get buffer !idx with
  | '\x1b' when len = 1 ->
    (* if we only read one byte and its an ESC, we just return Esc *)
    let key_event = KeyEvent { code = Esc; modifier = Normal } in
    Escape, Some key_event
  | '\x1b' -> Escape, None (* transition to escape sequence *)
  | '\x00' ->
    (* NULL byte is turned to ctrl + space *)
    let key_event = KeyEvent { code = Char ' '; modifier = Control } in
    Initial, Some key_event
  | '\x7F' ->
    (* byte value 127 maps to DEL in ascii, we turn into backspace *)
    let key_event = KeyEvent { code = Backspace; modifier = Normal } in
    Initial, Some key_event
  | '\x0D' ->
    (* byte 0x0D is CR carriage return *)
    let key_event = KeyEvent { code = Enter; modifier = Normal } in
    Initial, Some key_event
  | '\x0A' ->
    (* byte 0x0A is LF line feed *)
    let key_event = KeyEvent { code = Enter; modifier = Normal } in
    Initial, Some key_event
  | c when Char.to_int c >= 0x01 && Char.to_int c <= 0x1A ->
    (* byte is between 1 and 26 which means is a ctrl + a-z character *)
    let charcode = Char.to_int c - 1 + Char.to_int 'A' in
    let code = Char.of_int_exn charcode in
    let key_event = KeyEvent { code = Char code; modifier = Control } in
    Initial, Some key_event
  | c when Char.to_int c >= 0x1C && Char.to_int c <= 0x1F ->
    (* byte is between 28 and 31 which means its ctrl 4-7
       we can only detect those keys unfortunately. *)
    let charcode = Char.to_int c - 0x1C + Char.to_int '4' in
    let code = Char.of_int_exn charcode in
    let key_event = KeyEvent { code = Char code; modifier = Control } in
    Initial, Some key_event
  | c -> Initial, Some (KeyEvent { code = Char c; modifier = Normal })
;;

let escape_process ~buffer ~idx ~len =
  assert (!idx < len);
  let byte = Bytes.get buffer !idx in
  match !idx with
  | 1 ->
    (match byte with
     | '0' -> Escape, None (* this equivalent to ignoring the token *)
     | '[' -> failwith "not yet implemented"
     | '\x1B' ->
       (* when the bufer has \x1b\x1b twice in a row, it means esc *)
       let key_event = KeyEvent { code = Esc; modifier = Normal } in
       Escape, Some key_event
     | _ -> failwith "not yet implemented")
  | _ ->
    (match byte with
     (* all the seemingly random characters here are defined on ECMA-48
        and can be checked on the links on the top of this file *)
     | 'D' ->
       let key_event = KeyEvent { code = Left; modifier = Normal } in
       Escape, Some key_event
     | 'B' ->
       let key_event = KeyEvent { code = Down; modifier = Normal } in
       Escape, Some key_event
     | 'A' ->
       let key_event = KeyEvent { code = Up; modifier = Normal } in
       Escape, Some key_event
     | 'C' ->
       let key_event = KeyEvent { code = Right; modifier = Normal } in
       Escape, Some key_event
     | 'H' ->
       let key_event = KeyEvent { code = Home; modifier = Normal } in
       Escape, Some key_event
     | 'F' ->
       let key_event = KeyEvent { code = End; modifier = Normal } in
       Escape, Some key_event
     | c when Char.to_int c >= Char.to_int 'P' && Char.to_int c <= Char.to_int 'S' ->
       (* when the byte is between P and S (80 - 83) F1-F4 was pressed *)
       let function_num = Char.to_int c + 1 - Char.to_int 'P' in
       let key_event = KeyEvent { code = F function_num; modifier = Normal } in
       Escape, Some key_event
     | _ -> failwith "could not parse escape sequence")
;;

let rec process_buffer ~state ~buffer ~idx ~len =
  assert (!idx < len);
  let next_state, event =
    match state with
    | Initial -> initial_process ~buffer ~idx ~len
    | Escape -> escape_process ~buffer ~idx ~len
  in
  idx := !idx + 1;
  match event with
  | Some e -> e
  | None -> process_buffer ~state:next_state ~buffer ~idx ~len
;;

let read () =
  let buffer = Bytes.create tty_buffer_size in
  let rec loop () =
    let bytes_read = Core_unix.(read stdin ~buf:buffer ~pos:0 ~len:tty_buffer_size) in
    let idx = ref 0 in
    match bytes_read with
    | 0 -> loop ()
    | _ -> process_buffer ~state:Initial ~buffer ~idx ~len:bytes_read
  in
  loop ()
;;
