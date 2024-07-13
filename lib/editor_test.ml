open Editor
open Core

let%test "fill viewport respects position" =
  let open Viewport in
  let rows = 3 in
  let cols = 8 in
  let cells = Array.create ~len:(rows * cols) (make_cell ()) in
  let content = "Hello World!\ngoobye world!\nsend help!" in
  let text_object = Text_object.make content in
  let buffer = Text_buffer.make text_object 0 in
  let vp = ref { cells; rows; cols } in
  let result =
    fill_viewport buffer vp { col = 5; row = 0; width = cols; height = rows }
  in
  let styles = { fg = None; bg = None; bold = false } in
  let expect =
    { rows
    ; cols
    ; cells =
        [| { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = 'H'; styles }
         ; { symbol = 'e'; styles }
         ; { symbol = 'l'; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = 'g'; styles }
         ; { symbol = 'o'; styles }
         ; { symbol = 'o'; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = 's'; styles }
         ; { symbol = 'e'; styles }
         ; { symbol = 'n'; styles }
        |]
    }
  in
  [%eq: t] expect result
;;

let%test "should render correct ratios" =
  let open Viewport in
  let rows = 7 in
  let cols = 11 in
  let cells = Array.create ~len:(rows * cols) (make_cell ()) in
  let vp = { cells; rows; cols } in
  let buffer = Text_buffer.make (Text_object.make "Hello World") 0 in
  let pane_a = Pane.make ~buffer_id:0 ~id:0 in
  let pane_b = Pane.make ~buffer_id:0 ~id:1 in
  let pane_c = Pane.make ~buffer_id:0 ~id:2 in
  let branch =
    { direction = Horizontal
    ; ratios = [ 0.3; 0.3; 0.4 ]
    ; panes = [ Pane pane_a; Pane pane_b; Pane pane_c ]
    }
  in
  let tab = { panes = Branch branch; active_pane = 0 } in
  let editor =
    { tabs = [ tab ]; viewport = ref vp; active_tab = 0; buffers = [ buffer ] }
  in
  render_branch ~col:0 ~row:0 ~width:cols ~height:rows ~editor ~branch;
  let styles = { fg = None; bg = None; bold = false } in
  let expect =
    { rows
    ; cols
    ; cells =
        [| { symbol = 'H'; styles }
         ; { symbol = 'e'; styles }
         ; { symbol = 'l'; styles }
         ; { symbol = 'l'; styles }
         ; { symbol = 'o'; styles }
         ; { symbol = ' '; styles }
         ; { symbol = 'W'; styles }
         ; { symbol = 'o'; styles }
         ; { symbol = 'r'; styles }
         ; { symbol = 'l'; styles }
         ; { symbol = 'd'; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = 'H'; styles }
         ; { symbol = 'e'; styles }
         ; { symbol = 'l'; styles }
         ; { symbol = 'l'; styles }
         ; { symbol = 'o'; styles }
         ; { symbol = ' '; styles }
         ; { symbol = 'W'; styles }
         ; { symbol = 'o'; styles }
         ; { symbol = 'r'; styles }
         ; { symbol = 'l'; styles }
         ; { symbol = 'd'; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = ' '; styles }
         ; { symbol = 'H'; styles }
         ; { symbol = 'e'; styles }
         ; { symbol = 'l'; styles }
         ; { symbol = 'l'; styles }
         ; { symbol = 'o'; styles }
         ; { symbol = ' '; styles }
         ; { symbol = 'W'; styles }
         ; { symbol = 'o'; styles }
         ; { symbol = 'r'; styles }
         ; { symbol = 'l'; styles }
         ; { symbol = 'd'; styles }
        |]
    }
  in
  [%eq: Viewport.t] expect !(editor.viewport)
;;
