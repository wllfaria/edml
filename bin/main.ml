open Core
open Edml
open Types

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
  List.iter ~f:render_change @@ Viewport.diff ~prev ~curr
;;

let render_pane (pane : Pane.t) position (editor : Editor.t) ~viewport =
  let buffer = List.nth_exn editor.buffers pane.buffer_id in
  Viewport.fill buffer.text_object viewport position
;;

let distribute_dimension dimension ratios =
  let parts = List.map ratios ~f:(fun r -> r *. float_of_int dimension |> int_of_float) in
  let sum = List.fold parts ~init:0 ~f:( + ) in
  let remainder = dimension - sum in
  let len = List.length parts in
  List.mapi parts ~f:(fun idx p -> if idx = len - 1 then p + remainder else p)
;;

let rec render_split
  ~col:_
  ~row
  ~width
  ~height
  ~(split : Editor.pane_branch)
  ~(viewport : Viewport.t)
  ~(editor : Editor.t)
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
        ~create:(fun anchor h -> { col = 0; row = anchor; width; height = h })
    | Vertical ->
      let widths = distribute_dimension width split.ratios in
      make_position
        ~dimensions:widths
        ~selector:(fun pos -> pos.width + 1)
        ~create:(fun anchor w -> { col = anchor; row; width = w; height })
  in
  List.foldi split.panes ~init:viewport ~f:(fun idx vp pane ->
    let pane_pos = List.nth_exn positions idx in
    match pane with
    | Split b ->
      render_split
        ~col:pane_pos.col
        ~row:pane_pos.row
        ~width:pane_pos.width
        ~height:pane_pos.height
        ~split:b
        ~viewport:vp
        ~editor
    | Single p -> render_pane p pane_pos editor ~viewport:vp)
;;

let render_tab (tab : Editor.tab) (editor : Editor.t) =
  let vp = editor.viewport in
  let result =
    match tab.panes with
    | Split split ->
      render_split
        ~col:0
        ~row:0
        ~width:vp.cols
        ~height:vp.rows
        ~split
        ~editor
        ~viewport:editor.viewport
    | Single pane ->
      render_pane
        pane
        { col = 0; row = 0; width = vp.cols; height = vp.rows }
        editor
        ~viewport:editor.viewport
  in
  result
;;

let rec find_pane (node : Editor.pane_tree) needle =
  match node with
  | Single p when p.id = needle -> Some p
  | Split b -> List.find_map b.panes ~f:(fun subtree -> find_pane subtree needle)
  | _ -> None
;;

let rec event_loop (editor : Editor.t) =
  Ansi.Cursor.hide ();
  let previous_viewport = editor.viewport in
  let tab = List.nth_exn editor.tabs editor.active_tab in
  let editor = { editor with viewport = render_tab tab editor } in
  render_viewport_diffs previous_viewport editor.viewport;
  let pane =
    match find_pane tab.panes tab.active_pane with
    | Some p -> p
    | None -> failwith "unreachable"
  in
  let cursor = !(pane.cursor) in
  Ansi.Cursor.move_to ~col:cursor.col ~row:cursor.row;
  Ansi.Cursor.show ();
  Out_channel.flush stdout;
  let maybe_action =
    let open Event_handler in
    match Ansi.Event.read () with
    | KeyEvent key_event -> handle_key_event key_event editor.mode
    | _ -> failwith "lol"
  in
  (match maybe_action with
   | Some (Cursor cursor_action) ->
     let buffer = List.nth_exn editor.buffers pane.buffer_id in
     pane.cursor := Cursor.handle_action cursor_action cursor buffer.text_object
   | None -> ());
  event_loop editor
;;

let () =
  setup_terminal ();
  let args = Sys.get_argv () in
  let argc = Array.length args in
  let path = if argc >= 2 then Array.get args 1 else "" in
  let file_content =
    match Fs.maybe_read_file path with
    | Some content -> content
    | None -> ""
  in
  let text_object = Text_object.make file_content in
  let buffer_id = Utils.next_id ~id_ref:Utils.buffer_id in
  let buffer = Text_buffer.make text_object buffer_id in
  let pane_id = Utils.next_id ~id_ref:Utils.pane_id in
  let pane = Pane.make ~buffer_id:buffer.id ~id:pane_id in
  let dimensions = Ansi.Terminal.size () in
  let viewport = Viewport.make ~cols:dimensions.cols ~rows:dimensions.rows in
  let editor = Editor.make ~pane:(Single pane) ~active_pane:pane.id ~buffer ~viewport in
  let editor =
    { editor with viewport = render_tab (List.nth_exn editor.tabs 0) editor }
  in
  render_whole_viewport editor.viewport;
  event_loop editor
;;
