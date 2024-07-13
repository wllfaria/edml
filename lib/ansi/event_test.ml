open Core
open Event

let process_list ~list ~offset =
  List.rev
  @@ List.foldi list ~init:[] ~f:(fun idx results _ ->
    let buffer = Bytes.create 1024 in
    Bytes.fill buffer ~pos:0 ~len:1 (Char.of_int_exn (idx + offset));
    let len = 1 in
    let idx = ref 0 in
    let result = process_buffer ~state:Initial ~buffer ~idx ~len in
    result :: results)
;;

let make_ctrl_key_event ~list =
  List.fold list ~init:[] ~f:(fun events char ->
    let key_event = KeyEvent { code = Char char; modifier = Control } in
    key_event :: events)
;;

let rec make_chars ~from ~until ~acc =
  match from = until with
  | true -> acc
  | _ -> make_chars ~from:(from + 1) ~until ~acc:(Char.of_int_exn from :: acc)
;;

let%test "parse non escape sequence byte" =
  let expect = KeyEvent { code = Char 'a'; modifier = Normal } in
  let buffer = Bytes.create 1024 in
  Bytes.fill buffer ~pos:0 ~len:1 'a';
  let len = 1 in
  let idx = ref 0 in
  let result = process_buffer ~state:Initial ~buffer ~idx ~len in
  [%eq: event] result expect
;;

let%test "parse ctrl-a to ctrl-z" =
  (* we are creating a list of characters from A to Z *)
  let chars = make_chars ~from:(Char.to_int 'A') ~until:(Char.to_int 'Z' + 1) ~acc:[] in
  (* and converting this list to the expected output of parsing it *)
  let expect = make_ctrl_key_event ~list:chars in
  (* we parse each character index instead of the character itself as
     ctrl characters range from 1 to 26 *)
  let results = process_list ~list:chars ~offset:1 in
  (* after filtering out every item that differs from the result we should
     end with a few less items, this is due to the fact that some ctrl + key
     combinations map to other special keys, like ctrl + J being CR *)
  24
  = List.length
    @@ List.filter ~f:(fun (a, b) -> [%eq: event] a b)
    @@ List.zip_exn results expect
;;

let%test "parse ctrl-4 to ctrl-7" =
  (* we are creating a list of characters from 4 to 7 *)
  let chars = make_chars ~from:(Char.to_int '4') ~until:(Char.to_int '4' + 1) ~acc:[] in
  (* and converting this list to the expected output of parsing it *)
  let expect = make_ctrl_key_event ~list:chars in
  (* we parse each character index instead of the character itself as
     ctrl characters range from 28 to 31 *)
  let results = process_list ~list:chars ~offset:28 in
  (* after filtering out every item that differs from the result we should
     still have the same number of items as the chars list *)
  List.length chars
  = List.length
    @@ List.filter ~f:(fun (a, b) -> [%eq: event] a b)
    @@ List.zip_exn results expect
;;

let%test "parse null byte as ctrl-space" =
  let expect = KeyEvent { code = Char ' '; modifier = Control } in
  let buffer = Bytes.create 1024 in
  Bytes.fill buffer ~pos:0 ~len:1 '\x00';
  let len = 1 in
  let idx = ref 0 in
  let result = process_buffer ~state:Initial ~buffer ~idx ~len in
  [%eq: event] result expect
;;

let%test "parse 0x7F (127 - ASCII DEL) byte as backspace" =
  let expect = KeyEvent { code = Backspace; modifier = Normal } in
  let buffer = Bytes.create 1024 in
  Bytes.fill buffer ~pos:0 ~len:1 '\x7F';
  let len = 1 in
  let idx = ref 0 in
  let result = process_buffer ~state:Initial ~buffer ~idx ~len in
  [%eq: event] result expect
;;

let%test "parse \\n byte as enter" =
  let expect = KeyEvent { code = Enter; modifier = Normal } in
  let buffer = Bytes.create 1024 in
  Bytes.fill buffer ~pos:0 ~len:1 '\x0A';
  let len = 1 in
  let idx = ref 0 in
  let result = process_buffer ~state:Initial ~buffer ~idx ~len in
  [%eq: event] result expect
;;

let%test "parse \\r byte as enter" =
  let expect = KeyEvent { code = Enter; modifier = Normal } in
  let buffer = Bytes.create 1024 in
  Bytes.fill buffer ~pos:0 ~len:1 '\r';
  let len = 1 in
  let idx = ref 0 in
  let result = process_buffer ~state:Initial ~buffer ~idx ~len in
  [%eq: event] result expect
;;

let%test "parse arrow keys" =
  let expect =
    [ KeyEvent { code = Left; modifier = Normal }
    ; KeyEvent { code = Down; modifier = Normal }
    ; KeyEvent { code = Up; modifier = Normal }
    ; KeyEvent { code = Right; modifier = Normal }
    ]
  in
  let suffixes = [ 'D'; 'B'; 'A'; 'C' ] in
  (* Arrow keys are sent as \x1B0x where x is one of the suffixes above *)
  let results =
    List.map suffixes ~f:(fun s ->
      let buffer = Bytes.create 1024 in
      Bytes.fill buffer ~pos:0 ~len:1 '\x1B';
      Bytes.fill buffer ~pos:1 ~len:1 '0';
      Bytes.fill buffer ~pos:2 ~len:1 s;
      let len = 3 in
      let idx = ref 0 in
      process_buffer ~state:Initial ~buffer ~idx ~len)
  in
  let result =
    List.length
    @@ List.filter ~f:(fun (a, b) -> not ([%eq: event] a b))
    @@ List.zip_exn results expect
  in
  let expect = 0 in
  phys_equal expect result
;;

let%test "parse esc key possibility 1" =
  let expect = KeyEvent { code = Esc; modifier = Normal } in
  let buffer = Bytes.create 1024 in
  Bytes.fill buffer ~pos:0 ~len:1 '\x1B';
  Bytes.fill buffer ~pos:1 ~len:1 '\x1B';
  let len = 2 in
  let idx = ref 0 in
  let result = process_buffer ~state:Initial ~buffer ~idx ~len in
  [%eq: event] expect result
;;

let%test "parse esc key possibility 2" =
  let expect = KeyEvent { code = Esc; modifier = Normal } in
  let buffer = Bytes.create 1024 in
  Bytes.fill buffer ~pos:0 ~len:1 '\x1B';
  let len = 1 in
  let idx = ref 0 in
  let result = process_buffer ~state:Initial ~buffer ~idx ~len in
  [%eq: event] expect result
;;

let%test "parse home meta key" =
  let expect = KeyEvent { code = Home; modifier = Normal } in
  let buffer = Bytes.create 1024 in
  Bytes.fill buffer ~pos:0 ~len:1 '\x1B';
  Bytes.fill buffer ~pos:1 ~len:1 '0';
  Bytes.fill buffer ~pos:2 ~len:1 'H';
  let len = 3 in
  let idx = ref 0 in
  let result = process_buffer ~state:Initial ~buffer ~idx ~len in
  [%eq: event] expect result
;;

let%test "parse end meta key" =
  let expect = KeyEvent { code = End; modifier = Normal } in
  let buffer = Bytes.create 1024 in
  Bytes.fill buffer ~pos:0 ~len:1 '\x1B';
  Bytes.fill buffer ~pos:1 ~len:1 '0';
  Bytes.fill buffer ~pos:2 ~len:1 'F';
  let len = 3 in
  let idx = ref 0 in
  let result = process_buffer ~state:Initial ~buffer ~idx ~len in
  [%eq: event] expect result
;;

let%test "parse function keys F1 to F4" =
  let expect =
    [ KeyEvent { code = F 1; modifier = Normal }
    ; KeyEvent { code = F 2; modifier = Normal }
    ; KeyEvent { code = F 3; modifier = Normal }
    ; KeyEvent { code = F 4; modifier = Normal }
    ]
  in
  let suffixes = [ 'P'; 'Q'; 'R'; 'S' ] in
  let results =
    List.map suffixes ~f:(fun s ->
      let buffer = Bytes.create 1024 in
      Bytes.fill buffer ~pos:0 ~len:1 '\x1B';
      Bytes.fill buffer ~pos:1 ~len:1 '0';
      Bytes.fill buffer ~pos:2 ~len:1 s;
      let len = 3 in
      let idx = ref 0 in
      process_buffer ~state:Initial ~buffer ~idx ~len)
  in
  let result =
    List.length
    @@ List.filter ~f:(fun (a, b) -> not ([%eq: event] a b))
    @@ List.zip_exn results expect
  in
  let expect = 0 in
  phys_equal expect result
;;
