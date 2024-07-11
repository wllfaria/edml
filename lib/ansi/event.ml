let tty_buffer_size = 10

type keycode =
  | Char of char
  | Tab
  | Enter
  | Backspace
[@@deriving show { with_path = false }]

type key_modifier =
  | Control
  | Shift
  | Normal
[@@deriving show { with_path = false }]

type key_event =
  { code : keycode
  ; modifier : key_modifier
  }
[@@deriving show { with_path = false }]

type event = KeyEvent of key_event [@@deriving show { with_path = false }]

let read () =
  let buffer = Bytes.create tty_buffer_size in
  let rec loop () =
    let bytes_read = Unix.(read stdin buffer 0 tty_buffer_size) in
    match bytes_read with
    | 0 -> loop ()
    | _ ->
      (match Bytes.get buffer 0 with
       | '\x1b' -> failwith "not yet implemented"
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
