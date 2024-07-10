let tty_buffer_size = 10

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
       | c when Char.code c < 32 -> failwith "its a control"
       | '\r' -> failwith "its enter"
       | '\t' -> failwith "its a tab"
       | _ -> failwith "most likely a char")
  in
  loop ()
;;
