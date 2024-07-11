open Core
open Viewport

type editor =
  { panes : Pane.t ref list
  ; viewport : Viewport.t ref
  }

let handle_key_event key_event editor =
  let open Ansi.Event in
  let cursor_motion =
    match key_event with
    | { code = Char 'h'; _ } -> Cursor.MoveLeft
    | { code = Char 'j'; _ } -> Cursor.MoveDown
    | { code = Char 'k'; _ } -> Cursor.MoveUp
    | { code = Char 'l'; _ } -> Cursor.MoveRight
    | _ -> failwith "Unhandled key event"
  in
  let pane = List.nth_exn editor.panes 0 in
  let new_cursor = Cursor.handle_action !(!pane.cursor) cursor_motion in
  !pane.cursor := new_cursor
;;

let print_whole_viewport (viewport : Viewport.t) =
  let changes = Viewport.to_changes viewport in
  List.iter changes ~f:(fun change ->
    Ansi.Cursor.move_to change.col change.row;
    Printf.printf "%c" change.cell.symbol)
;;

let fill_viewport (buffer : Text_buffer.t ref) viewport =
  List.foldi !buffer.text_object.content ~init:viewport ~f:(fun row vp line ->
    if row < vp.rows
    then
      String.foldi line ~init:vp ~f:(fun col vp char ->
        if col < vp.cols then Viewport.set_cell char ~col ~row ~vp else vp)
    else vp)
;;

let rec event_loop editor =
  let pane = List.nth_exn editor.panes 0 in
  let buffer = !pane.buffer in
  editor.viewport := fill_viewport buffer !(editor.viewport);
  print_whole_viewport !(editor.viewport);
  (match Ansi.Event.read () with
   | KeyEvent key_event -> handle_key_event key_event editor);
  let cursor = !(pane.contents.cursor) in
  Ansi.Cursor.move_to cursor.col cursor.row;
  Out_channel.flush stdout;
  event_loop editor
;;

let run () =
  let filename = (Sys.get_argv ()).(1) in
  let content = Fs.read_file filename in
  let text_object = Text_object.make content in
  let buffer = ref @@ Text_buffer.make text_object in
  let viewport = ref @@ Viewport.make ~cols:80 ~rows:40 in
  let pane = Pane.make buffer in
  let editor = { panes = [ ref pane ]; viewport } in
  event_loop editor
;;

let%test "should fill the viewport" =
  let open Viewport in
  let default_style = { fg = None; bg = None; bold = false } in
  let expect =
    { cells =
        [ { symbol = 'h'; styles = default_style }
        ; { symbol = 'e'; styles = default_style }
        ; { symbol = 'l'; styles = default_style }
        ; { symbol = 'l'; styles = default_style }
        ; { symbol = 'o'; styles = default_style }
        ; { symbol = '!'; styles = default_style }
        ; { symbol = ' '; styles = default_style }
        ; { symbol = ' '; styles = default_style }
        ; { symbol = ' '; styles = default_style }
        ]
    ; rows = 1
    ; cols = 9
    }
  in
  let content = "hello!" in
  let text_object = Text_object.make content in
  let buffer = ref @@ Text_buffer.make text_object in
  let viewport = ref @@ Viewport.make ~cols:9 ~rows:1 in
  let pane = Pane.make buffer in
  let editor = { panes = [ ref pane ]; viewport } in
  let result = fill_viewport buffer !(editor.viewport) in
  editor.viewport := result;
  [%eq: Viewport.t] !(editor.viewport) expect
;;
