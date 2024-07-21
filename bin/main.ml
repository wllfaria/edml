open Core
open Edml
open Assertions
open Types
open Viewport

let setup_terminal _ =
  Ansi.Terminal.enable_raw_mode ();
  Ansi.Terminal.clear_screen ();
  Ansi.Terminal.enter_alternate_screen ();
  Ansi.Cursor.move_to ~col:0 ~row:0
;;

let render_change (change : Viewport.change) =
  Ansi.Cursor.move_to ~col:change.col ~row:change.row;
  Fmt.pr "%c" change.cell.symbol
;;

let render_whole_viewport viewport =
  Array.iter ~f:render_change @@ Viewport.to_changes viewport
;;

let render_viewport_diffs prev curr =
  let difs = Viewport.diff ~prev ~curr in
  List.iter ~f:render_change @@ difs
;;

let render_pane ~(pane : Pane.pane) ~position ~(editor : Editor.editor) ~cursor =
  let buffer = List.nth_exn editor.buffers pane.buffer_id in
  Viewport.fill !(buffer.text_object) cursor editor.viewport position
;;

let distribute_dimension dimension ratios =
  let parts = List.map ratios ~f:(fun r -> r *. float_of_int dimension |> int_of_float) in
  let sum = List.fold parts ~init:0 ~f:( + ) in
  let remainder = dimension - sum in
  let len = List.length parts in
  List.mapi parts ~f:(fun idx p -> if idx = len - 1 then p + remainder else p)
;;

let rec render_split
  ~col
  ~row
  ~width
  ~height
  ~(split : Editor.pane_branch)
  ~(editor : Editor.editor)
  ~cursor
  =
  let make_position ~dimensions ~selector ~create =
    List.fold dimensions ~init:[] ~f:(fun acc d ->
      let sum : int = List.fold acc ~init:0 ~f:(fun acc pos -> acc + selector pos) in
      create sum d :: acc)
  in
  let positions =
    match split.direction with
    | Horizontal ->
      let heights = distribute_dimension height split.ratios in
      make_position
        ~dimensions:heights
        ~selector:(fun pos -> pos.height + 1)
        ~create:(fun anchor h -> { col; row = anchor; width; height = h })
    | Vertical ->
      let widths = distribute_dimension width split.ratios in
      make_position
        ~dimensions:widths
        ~selector:(fun pos -> pos.width + 1)
        ~create:(fun anchor w -> { col = anchor; row; width = w; height })
  in
  List.iteri split.panes ~f:(fun idx pane ->
    let position = List.nth_exn positions idx in
    match pane with
    | Split split ->
      render_split
        ~col:position.col
        ~row:position.row
        ~width:position.width
        ~height:position.height
        ~split
        ~editor
        ~cursor
    | Single pane -> render_pane ~pane ~position ~editor ~cursor)
;;

let render_tab (tab : Editor.tab) (editor : Editor.editor) cursor =
  let vp = editor.viewport in
  match tab.panes with
  | Split split ->
    render_split ~col:0 ~row:0 ~width:!vp.cols ~height:!vp.rows ~split ~editor ~cursor
  | Single pane ->
    let position = { col = 0; row = 0; width = !vp.cols; height = !vp.rows } in
    render_pane ~pane ~position ~editor ~cursor
;;

let rec find_pane (node : Editor.pane_tree) needle =
  match node with
  | Single p when p.id = needle -> Some p
  | Split b -> List.find_map b.panes ~f:(fun subtree -> find_pane subtree needle)
  | _ -> None
;;

let handle_action ~(editor : Editor.editor) ~(pane : Pane.pane) =
  let dimensions : Ansi.Terminal.dimensions =
    { rows = !(editor.viewport).rows; cols = !(editor.viewport).cols }
  in
  let maybe_actions =
    match Ansi.Event.read () with
    | KeyEvent key_event -> Keymaps.Event_handler.handle_key_event key_event editor.mode
    | _ -> todo ()
  in
  let editor =
    match maybe_actions with
    | Some actions ->
      List.fold actions ~init:editor ~f:(fun acc action ->
        match action with
        | CursorAction cursor_action ->
          let buffer = List.nth_exn acc.buffers pane.buffer_id in
          let text_object = !(buffer.text_object) in
          pane.cursor
          := Cursor.handle_action cursor_action !(pane.cursor) text_object dimensions;
          acc
        | ChangeMode mode -> { acc with mode }
        | TextObjectAction action ->
          let buffer = List.nth_exn editor.buffers pane.buffer_id in
          let cursor = !(pane.cursor) in
          let anchor = { col = cursor.col; row = cursor.row } in
          let text_object = !(buffer.text_object) in
          buffer.text_object := Text_object.handle_action ~action ~text_object ~anchor;
          pane.cursor := Cursor.move_right !(pane.cursor) text_object dimensions;
          acc)
    | None -> editor
  in
  editor
;;

let render_cursor_in_view ~cursor =
  let open Cursor in
  let row = cursor.row - cursor.offset_row in
  let col = cursor.col - cursor.offset_col in
  Ansi.Cursor.move_to ~col ~row
;;

let rec event_loop (editor : Editor.editor) =
  Ansi.Cursor.hide ();
  let previous_viewport = Viewport.copy !(editor.viewport) in
  let tab = List.nth_exn editor.tabs editor.active_tab in
  let pane =
    match find_pane tab.panes tab.active_pane with
    | Some p -> p
    | None -> unreachable ()
  in
  let editor = handle_action ~editor ~pane in
  let cursor = !(pane.cursor) in
  render_tab tab editor cursor;
  render_viewport_diffs previous_viewport !(editor.viewport);
  render_cursor_in_view ~cursor;
  Ansi.Cursor.show ();
  Out_channel.flush stdout;
  event_loop editor
;;

let () =
  setup_terminal ();
  let args = Sys.get_argv () in
  let argc = Array.length args in
  let path = if argc >= 2 then Array.get args 1 else "" in
  let editor = Editor.init path in
  let pane = Editor.get_focused_pane editor in
  render_tab (List.nth_exn editor.tabs 0) editor !(pane.cursor);
  render_whole_viewport !(editor.viewport);
  render_cursor_in_view ~cursor:!(pane.cursor);
  Out_channel.flush stdout;
  event_loop editor
;;
