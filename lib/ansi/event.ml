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
      let bytes = Bytes.sub_string buffer 0 bytes_read in
      (match bytes.[0] with
       | '\x1b' -> failwith "not yet implemented"
       | '\x7f' -> KeyEvent { code = Backspace; modifier = Normal }
       | '\r' -> KeyEvent { code = Enter; modifier = Normal }
       | '\t' -> KeyEvent { code = Tab; modifier = Normal }
       | c when Char.code c > 0x01 && Char.code c < 0x1A ->
         KeyEvent
           { code = Char (Char.chr (Char.code c - 0x01 + Char.code 'a'))
           ; modifier = Control
           }
       | c when Char.code c > 0x1c && Char.code c < 0x1f ->
         KeyEvent
           { code = Char (Char.chr (Char.code c - 0x1c + Char.code '4'))
           ; modifier = Control
           }
       | c when Char.code c < 32 -> failwith "its a control"
       | _ -> failwith "most likely a char")
  in
  loop ()
;;
