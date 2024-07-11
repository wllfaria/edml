open Core
open Pane
open Viewport

let todo () = failwith "Not yet implemented!"

type pane_tree =
  | Pane of Pane.t
  | Branch of pane_branch

and pane_branch =
  { direction : direction
  ; children : pane_tree
  }

and direction =
  | Horizontal
  | Vertical

type editor =
  { panes : pane_tree
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
    | _ -> todo ()
  in
  let pane =
    match editor.panes with
    | Pane p -> p
    | Branch _ -> todo ()
  in
  let new_cursor = Cursor.handle_action !(pane.cursor) cursor_motion in
  pane.cursor := new_cursor
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
  let pane =
    match editor.panes with
    | Pane p -> p
    | Branch _ -> todo ()
  in
  let buffer = pane.buffer in
  editor.viewport := fill_viewport buffer !(editor.viewport);
  print_whole_viewport !(editor.viewport);
  let cursor = !(pane.cursor) in
  Ansi.Cursor.move_to cursor.col cursor.row;
  Out_channel.flush stdout;
  (match Ansi.Event.read () with
   | KeyEvent key_event -> handle_key_event key_event editor);
  event_loop editor
;;

let run () =
  let filename = (Sys.get_argv ()).(1) in
  let content = Fs.read_file filename in
  let text_object = Text_object.make content in
  let buffer = ref @@ Text_buffer.make text_object in
  let size = Ansi.Terminal.size () in
  print_endline @@ Ansi.Terminal.show_dimensions size;
  let viewport = ref @@ Viewport.make ~cols:size.cols ~rows:size.rows in
  let position = { col = 0; row = 0; width = size.cols; height = size.rows } in
  let pane = Pane.make buffer position in
  let editor = { panes = Pane pane; viewport } in
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
  let pane = Pane.make buffer { col = 0; row = 0; width = 9; height = 1 } in
  let editor = { panes = Pane pane; viewport } in
  let result = fill_viewport buffer !(editor.viewport) in
  editor.viewport := result;
  [%eq: Viewport.t] !(editor.viewport) expect
;;
