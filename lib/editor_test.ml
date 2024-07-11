open Editor

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
