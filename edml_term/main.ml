open Core
open Edml
open Assertions
open Types
open Edml_term_lib
open Viewport
open Base
open Ansi.Command

let restore_terminal () =
  Ansi.Terminal.disable_raw_mode ();
  execute [ Terminal LeaveAlternateScreen; Cursor Show ];
  exit 0
;;

let setup_interrupt_signal () =
  try
    let _ = Signal.Expert.(signal Signal.int (`Handle (fun _ -> restore_terminal ()))) in
    ()
  with
  | Core_unix.Unix_error _ -> ()
;;

let setup_terminal () =
  Ansi.Terminal.enable_raw_mode ();
  execute [ Terminal EnterAlternateScreen; Cursor (MoveTo (0, 0)) ];
  setup_interrupt_signal ()
;;

let setup_logger () = Logger.init "/home/wiru/code/edml/edml.log" Async

let render_change (change : Viewport.change) =
  queue
    [ Cursor (MoveTo (change.col, change.row))
    ; SetForegroundColor change.cell.styles.fg
    ; SetBackgroundColor change.cell.styles.bg
    ; Print (Char.to_string change.cell.symbol)
    ]
;;

let render_whole_viewport viewport =
  Array.iter ~f:render_change @@ Viewport.to_changes viewport
;;

let render_viewport_diffs prev curr =
  let difs = Viewport.diff ~prev ~curr in
  List.iter ~f:render_change @@ difs
;;

let render_pane viewport ~(pane : Pane.pane) ~position ~(editor : Base.editor) ~cursor =
  let buffer = List.nth_exn editor.buffers pane.buffer_id in
  Viewport.fill !(buffer.text_object) cursor viewport position buffer.matches
;;

let distribute_dimension dimension ratios =
  let parts = List.map ratios ~f:(fun r -> r *. float_of_int dimension |> int_of_float) in
  let sum = List.fold parts ~init:0 ~f:( + ) in
  let remainder = dimension - sum in
  let len = List.length parts in
  List.mapi parts ~f:(fun idx p -> if idx = len - 1 then p + remainder else p)
;;

let rec render_split
  viewport
  ~col
  ~row
  ~width
  ~height
  ~(split : Base.pane_branch)
  ~(editor : Base.editor)
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
        viewport
        ~col:position.col
        ~row:position.row
        ~width:position.width
        ~height:position.height
        ~split
        ~editor
        ~cursor
    | Single pane -> render_pane viewport ~pane ~position ~editor ~cursor)
;;

let render_tab viewport (tab : Base.tab) (editor : Base.editor) (cursor : Cursor.cursor) =
  let vp = viewport in
  match tab.panes with
  | Split split ->
    render_split vp ~col:0 ~row:0 ~width:!vp.cols ~height:!vp.rows ~split ~editor ~cursor
  | Single pane ->
    let position = { col = 0; row = 0; width = !vp.cols; height = !vp.rows } in
    render_pane vp ~pane ~position ~editor ~cursor
;;

let rec find_pane (node : Base.pane_tree) needle =
  match node with
  | Single p when p.id = needle -> Some p
  | Split b -> List.find_map b.panes ~f:(fun subtree -> find_pane subtree needle)
  | _ -> None
;;

let handle_action ~viewport ~(editor : Base.editor) ~(pane : Pane.pane) =
  let maybe_actions =
    match Ansi_to_editor.event_of_ansi @@ Ansi.Event.read () with
    | KeyEvent key_event -> Event_handler.handle_key_event key_event editor.mode
    | Resize new_size ->
      viewport := Viewport.from_dimensions new_size;
      None
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
          := Cursor.handle_action
               cursor_action
               !(pane.cursor)
               text_object
               (Ansi.Terminal.size ());
          acc
        | ChangeMode mode -> { acc with mode }
        | TextObjectAction action ->
          let buffer = List.nth_exn editor.buffers pane.buffer_id in
          let cursor = !(pane.cursor) in
          let anchor = { col = cursor.col; row = cursor.row } in
          let text_object = !(buffer.text_object) in
          buffer.text_object := Text_object.handle_action ~action ~text_object ~anchor;
          pane.cursor
          := Cursor.move_right !(pane.cursor) text_object (Ansi.Terminal.size ());
          acc)
    | None -> editor
  in
  editor
;;

let render_cursor_in_view ~cursor =
  let open Cursor in
  let row = cursor.row - cursor.offset_row in
  let col = cursor.col - cursor.offset_col in
  queue [ Cursor (MoveTo (col, row)) ]
;;

let rec event_loop viewport editor =
  let previous_viewport = Viewport.copy !viewport in
  let tab = List.nth_exn editor.tabs editor.active_tab in
  let pane = expect ~msg:"must have a pane" @@ find_pane tab.panes tab.active_pane in
  let editor = handle_action ~viewport ~editor ~pane in
  let cursor = !(pane.cursor) in
  execute [ Cursor Hide ];
  render_tab viewport tab editor cursor;
  render_viewport_diffs previous_viewport !viewport;
  render_cursor_in_view ~cursor;
  queue [ Cursor Show ];
  flush ();
  if editor.quitting then () else event_loop viewport editor
;;

let () =
  setup_terminal ();
  setup_logger ();
  Load_colorscheme.load_colors ();
  let args = Sys.get_argv () in
  let argc = Array.length args in
  let path = if argc >= 2 then Array.get args 1 else "" in
  let editor = Base.init path in
  let pane = Base.get_focused_pane editor in
  let dimensions = Ansi.Terminal.size () in
  let viewport = ref @@ Viewport.make ~cols:dimensions.cols ~rows:dimensions.rows in
  render_tab viewport (List.nth_exn editor.tabs 0) editor !(pane.cursor);
  render_whole_viewport !viewport;
  render_cursor_in_view ~cursor:!(pane.cursor);
  flush ();
  event_loop viewport editor;
  restore_terminal ()
;;
