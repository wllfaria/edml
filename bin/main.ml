let () =
  Ansi.Terminal.enable_raw_mode ();
  Ansi.Terminal.clear_screen ();
  Ansi.Cursor.move_to 0 1;
  Ansi.Terminal.enter_alternate_screen ();
  flush stdout;
  let line = ref 1 in
  let rec loop () =
    Ansi.Cursor.move_to 0 !line;
    let result = Ansi.Event.read () in
    print_endline @@ Ansi.Event.show_event result;
    line := !line + 1;
    Ansi.Cursor.move_to 0 !line;
    flush stdout;
    loop ()
  in
  loop ()
;;
