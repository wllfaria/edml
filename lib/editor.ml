open Core
open Utils
open Types
open Pane
open Viewport
open Text_buffer

type editor =
  { tabs : tab list
  ; viewport : Viewport.t ref
  ; buffers : Text_buffer.t list
  ; active_tab : int
  }

and anchor =
  { col : int
  ; row : int
  }

and tab =
  { panes : pane_tree
  ; active_pane : int
  }

and pane_tree =
  | Pane of Pane.t
  | Branch of pane_branch

and pane_branch =
  { direction : direction
  ; ratios : float list
  ; panes : pane_tree list
  }

and direction =
  | Horizontal
  | Vertical

let render_change (change : change) =
  Ansi.Cursor.move_to ~col:change.col ~row:change.row;
  Printf.printf "%c" change.cell.symbol
;;

let render_whole_viewport viewport = Array.iter ~f:render_change @@ to_changes viewport
let render_viewport_diffs prev curr = List.iter ~f:render_change @@ diff ~prev ~curr

let render_pane pane position editor =
  let buffer = List.nth_exn editor.buffers pane.buffer_id in
  let viewport = editor.viewport in
  Viewport.fill buffer viewport position
;;

let distribute_dimension dimension ratios =
  let parts = List.map ratios ~f:(fun r -> r *. float_of_int dimension |> int_of_float) in
  let sum = List.fold parts ~init:0 ~f:( + ) in
  let remainder = dimension - sum in
  let len = List.length parts in
  List.mapi parts ~f:(fun idx p -> if idx = len - 1 then p + remainder else p)
;;

let rec render_branch ~col:_ ~row ~width ~height ~branch ~editor =
  let make_position ~dimensions ~selector ~create =
    List.fold dimensions ~init:[] ~f:(fun acc d ->
      let sum : int = List.fold acc ~init:0 ~f:(fun acc pos -> acc + selector pos) in
      create sum d :: acc)
  in
  let positions =
    match branch.direction with
    | Horizontal ->
      let heights = distribute_dimension height branch.ratios in
      make_position
        ~dimensions:heights
        ~selector:(fun pos -> pos.height + 1)
        ~create:(fun anchor h -> { col = 0; row = anchor; width; height = h })
    | Vertical ->
      let widths = distribute_dimension width branch.ratios in
      make_position
        ~dimensions:widths
        ~selector:(fun pos -> pos.width + 1)
        ~create:(fun anchor w -> { col = anchor; row; width = w; height })
  in
  List.iteri branch.panes ~f:(fun idx pane ->
    let pane_pos = List.nth_exn positions idx in
    match pane with
    | Branch b ->
      render_branch
        ~col:pane_pos.col
        ~row:pane_pos.row
        ~width:pane_pos.width
        ~height:pane_pos.height
        ~branch:b
        ~editor
    | Pane p -> editor.viewport := render_pane p pane_pos editor)
;;

let render_tab tab editor =
  let vp = !(editor.viewport) in
  match tab.panes with
  | Branch b ->
    render_branch ~col:0 ~row:0 ~width:vp.cols ~height:vp.rows ~branch:b ~editor
  | Pane p ->
    editor.viewport
    := render_pane p { col = 0; row = 0; width = vp.cols; height = vp.rows } editor
;;

let rec find_pane node needle =
  match node with
  | Pane p when p.id = needle -> Some p
  | Branch b -> List.find_map b.panes ~f:(fun subtree -> find_pane subtree needle)
  | _ -> None
;;

let rec event_loop editor =
  Ansi.Cursor.hide ();
  let previous_viewport = !(editor.viewport) in
  let tab = List.nth_exn editor.tabs editor.active_tab in
  render_tab tab editor;
  render_viewport_diffs previous_viewport !(editor.viewport);
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
    | KeyEvent key_event -> handle_key_event key_event
    | _ -> failwith "lol"
  in
  (match maybe_action with
   | Some (Cursor cursor_action) ->
     let buffer = List.nth_exn editor.buffers pane.buffer_id in
     pane.cursor := Cursor.handle_action cursor_action cursor buffer.text_object
   | None -> ());
  event_loop editor
;;

let run () =
  let filename = (Sys.get_argv ()).(1) in
  let content = Fs.read_file filename in
  let text_object = Text_object.make content in
  let buffer = Text_buffer.make text_object @@ next_id ~id_ref:buffer_id in
  let size = Ansi.Terminal.size () in
  let viewport = ref @@ Viewport.make ~cols:size.cols ~rows:size.rows in
  let buffers = [ buffer ] in
  let pane = Pane.make ~buffer_id:buffer.id ~id:(next_id ~id_ref:pane_id) in
  let tab = { panes = Pane pane; active_pane = pane.id } in
  let tabs = [ tab ] in
  let editor = { buffers; tabs; viewport; active_tab = 0 } in
  render_tab (List.nth_exn editor.tabs 0) editor;
  render_whole_viewport !(editor.viewport);
  event_loop editor
;;
