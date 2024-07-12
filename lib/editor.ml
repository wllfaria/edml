open Core
open Pane
open Viewport
open Text_buffer

let todo () = failwith "Not yet implemented!"

type position =
  { row : int
  ; col : int
  ; width : int
  ; height : int
  }
[@@deriving show { with_path = false }]

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

and tab = { panes : pane_tree }

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

let render_change (change : change) =
  Ansi.Cursor.move_to change.col change.row;
  Printf.printf "%c" change.cell.symbol
;;

let render_whole_viewport viewport = List.iter ~f:render_change @@ to_changes viewport
(* let render_viewport_diffs prev curr = List.iter ~f:render_change @@ diff ~prev ~curr *)

let fill_viewport buffer viewport (position : position) =
  let result =
    List.foldi buffer.text_object.content ~init:!viewport ~f:(fun row vp line ->
      let row = position.row + row in
      if row < vp.rows && row < position.row + position.height
      then
        String.foldi line ~init:vp ~f:(fun col vp char ->
          let col = col + position.col in
          if col < vp.cols && col < position.col + position.width
          then set_cell char ~col ~row ~vp
          else vp)
      else vp)
  in
  result
;;

(* let rec event_loop editor = *)
(*   let pane = *)
(*     match editor.panes with *)
(*     | Pane p -> p *)
(*     | Branch _ -> todo () *)
(*   in *)
(*   let buffer = pane.buffer in *)
(*   let previous_viewport = !(editor.viewport) in *)
(*   editor.viewport := fill_viewport buffer !(editor.viewport); *)
(*   render_viewport_diffs previous_viewport !(editor.viewport); *)
(*   let cursor = !(pane.cursor) in *)
(*   Ansi.Cursor.move_to cursor.col cursor.row; *)
(*   Out_channel.flush stdout; *)
(*   (match Ansi.Event.read () with *)
(*    | KeyEvent key_event -> handle_key_event key_event editor); *)
(*   event_loop editor *)
(* ;; *)

let buffer_id = ref 0

let next_buffer_id () =
  let id = !buffer_id in
  buffer_id := !buffer_id + 1;
  id
;;

let render_pane pane position editor =
  let buffer = List.nth_exn editor.buffers pane.buffer_id in
  let viewport = editor.viewport in
  fill_viewport buffer viewport position
;;

let distribute_dimension dimension ratios =
  let parts =
    List.map ratios ~f:(fun ratio -> ratio *. float_of_int dimension |> int_of_float)
  in
  let sum = List.fold parts ~init:0 ~f:( + ) in
  let remainder = dimension - sum in
  List.mapi parts ~f:(fun idx w -> if idx < remainder then w + 1 else w)
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

let run () =
  let filename = (Sys.get_argv ()).(1) in
  let content = Fs.read_file filename in
  let content_b =
    Fs.read_file "../hac/hac-client/src/pages/collection_viewer/collection_viewer.rs"
  in
  let text_object = Text_object.make content in
  let text_object_b = Text_object.make content_b in
  let buffer = Text_buffer.make text_object @@ next_buffer_id () in
  let buffer_b = Text_buffer.make text_object_b @@ next_buffer_id () in
  let size = Ansi.Terminal.size () in
  let viewport = ref @@ Viewport.make ~cols:size.cols ~rows:size.rows in
  let buffers = [ buffer; buffer_b ] in
  let pane = Pane.make buffer.id in
  let pane_b = Pane.make buffer_b.id in
  let tab =
    { panes =
        Branch
          { direction = Vertical
          ; panes =
              [ Pane pane
              ; Branch
                  { panes = [ Pane pane; Pane pane_b ]
                  ; direction = Horizontal
                  ; ratios = [ 0.5; 0.5 ]
                  }
              ]
          ; ratios = [ 0.5; 0.5 ]
          }
    }
  in
  let tabs = [ tab ] in
  let editor = { buffers; tabs; viewport; active_tab = 0 } in
  render_tab (List.nth_exn editor.tabs 0) editor;
  render_whole_viewport !(editor.viewport);
  Out_channel.flush stdout
;;

(* let rec loop () = loop () in *)
(* loop () *)

(* let pane = Pane.make buffer in *)
(* let pane_b = Pane.make buffer_b in *)
(* let panes = Branch { direction = Horizontal; panes = [ Pane pane; Pane pane_b ] } in *)
(* let tab = { panes } in *)
(* let editor = { tabs = [ tab ]; viewport } in *)
(* editor.viewport := fill_viewport buffer !(editor.viewport); *)
(* render_whole_viewport !(editor.viewport); *)
(* event_loop editor *)
