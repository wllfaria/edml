open Core
open Edml

let%test "fill viewport respects position" =
  let open Cursor in
  let open Viewport in
  let rows = 3 in
  let cols = 8 in
  let content = "Hello World!\ngoodbye world!\nsend help!" in
  let text_object = Text_object.make content in
  let vp = ref @@ Viewport.make ~rows ~cols in
  let cursor = { row = 0; col = 0; real_col = 0; offset_col = 0; offset_row = 0 } in
  Viewport.fill text_object cursor vp { col = 5; row = 0; width = cols; height = rows };
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
  [%eq: t] expect !vp
;;
