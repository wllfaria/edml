let () =
  Ansi.Terminal.enable_raw_mode ();
  Ansi.Terminal.clear_screen ();
  Ansi.Terminal.enter_alternate_screen ();
  Ansi.Cursor.move_to 1 1;
  flush stdout;
  Edml.Editor.run ()
;;
