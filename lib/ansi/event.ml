let tty_buffer_size = 10

type event =
  | FocusGained
  | FocusLost
  | KeyEvent of key_event
[@@deriving show { with_path = false }]

and key_event =
  { code : keycode
  ; modifier : key_modifier
  }
[@@deriving show { with_path = false }]

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
[@@deriving show { with_path = false }]

and key_modifier =
  | Control
  | Shift
  | Normal
[@@deriving show { with_path = false }]

let parse_csi buffer =
  if Bytes.length buffer = 2
  then KeyEvent { code = Esc; modifier = Normal }
  else (
    match Bytes.get buffer 2 with
    | 'D' -> KeyEvent { code = Left; modifier = Normal }
    | 'C' -> KeyEvent { code = Right; modifier = Normal }
    | 'A' -> KeyEvent { code = Up; modifier = Normal }
    | 'B' -> KeyEvent { code = Down; modifier = Normal }
    | 'H' -> KeyEvent { code = Home; modifier = Normal }
    | 'F' -> KeyEvent { code = End; modifier = Normal }
    | 'Z' -> KeyEvent { code = BackTab; modifier = Shift }
    | 'I' -> FocusGained
    | 'O' -> FocusLost
    | _ -> failwith "could not parse csi event")
;;

let read () =
  let buffer = Bytes.create tty_buffer_size in
  let rec loop () =
    let bytes_read = Unix.(read stdin buffer 0 tty_buffer_size) in
    match bytes_read with
    | 0 -> loop ()
    | _ ->
      (match Bytes.get buffer 0 with
       | '\x1b' ->
         (match Bytes.length buffer with
          | 1 -> KeyEvent { code = Esc; modifier = Normal }
          | _ -> parse_csi buffer)
       | '\x7f' -> KeyEvent { code = Backspace; modifier = Normal }
       | '\r' -> KeyEvent { code = Enter; modifier = Normal }
       | '\t' -> KeyEvent { code = Tab; modifier = Normal }
       | '\x00' -> KeyEvent { code = Char ' '; modifier = Control }
       | c when Char.code c > 0x01 && Char.code c < 0x1A ->
         KeyEvent
           { code = Char (Char.chr @@ (Char.code c - 0x01 + Char.code 'a'))
           ; modifier = Control
           }
       | c when Char.code c > 0x1c && Char.code c < 0x1f ->
         KeyEvent
           { code = Char (Char.chr @@ (Char.code c - 0x1c + Char.code '4'))
           ; modifier = Control
           }
       | c when Char.code c < 32 ->
         KeyEvent { code = Char (Char.chr @@ (Char.code c + 96)); modifier = Control }
       | c -> KeyEvent { code = Char c; modifier = Normal })
  in
  loop ()
;;
