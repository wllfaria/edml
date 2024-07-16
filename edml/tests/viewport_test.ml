open Core
open Edml.Viewport

let%test "should create correct sized viewport" =
  let expected_len = 100 in
  let result = make ~cols:10 ~rows:10 in
  phys_equal expected_len (Array.length result.cells)
;;

let%test "should get correct diffs" =
  let sample_cell = { symbol = 'x'; styles = { fg = None; bg = None; bold = false } } in
  let expected =
    [ { cell = sample_cell; col = 0; row = 0 }
    ; { cell = sample_cell; col = 2; row = 0 }
    ; { cell = sample_cell; col = 1; row = 1 }
    ; { cell = sample_cell; col = 2; row = 1 }
    ]
  in
  let prev = make ~cols:3 ~rows:2 in
  let curr =
    { cols = 3
    ; rows = 2
    ; cells =
        [| sample_cell
         ; make_cell ()
         ; sample_cell
         ; make_cell ()
         ; sample_cell
         ; sample_cell
        |]
    }
  in
  let result = diff ~prev ~curr in
  [%eq: change list] expected result
;;

let%test "should make new vp with correct text" =
  let base_style = { fg = None; bg = None; bold = false } in
  let expect =
    { cells =
        [| { symbol = ' '; styles = base_style }
         ; { symbol = ' '; styles = base_style }
         ; { symbol = ' '; styles = base_style }
         ; { symbol = ' '; styles = base_style }
         ; { symbol = ' '; styles = base_style }
         ; { symbol = ' '; styles = base_style }
         ; { symbol = ' '; styles = base_style }
         ; { symbol = ' '; styles = base_style }
         ; { symbol = ' '; styles = base_style }
         ; { symbol = ' '; styles = base_style }
         ; { symbol = ' '; styles = base_style }
         ; { symbol = ' '; styles = base_style }
         ; { symbol = 'H'; styles = base_style }
         ; { symbol = 'e'; styles = base_style }
         ; { symbol = 'l'; styles = base_style }
         ; { symbol = 'l'; styles = base_style }
         ; { symbol = 'o'; styles = base_style }
         ; { symbol = ','; styles = base_style }
         ; { symbol = ' '; styles = base_style }
         ; { symbol = 'W'; styles = base_style }
         ; { symbol = 'o'; styles = base_style }
         ; { symbol = 'r'; styles = base_style }
         ; { symbol = 'l'; styles = base_style }
         ; { symbol = 'd'; styles = base_style }
         ; { symbol = '!'; styles = base_style }
         ; { symbol = ' '; styles = base_style }
        |]
    ; rows = 2
    ; cols = 13
    }
  in
  let vp = make ~cols:13 ~rows:2 in
  let result = set_text "Hello, World!" ~col:12 ~row:0 ~vp in
  [%eq: t] result expect
;;

let%test "should make new vp with correct cell" =
  let base_style = { fg = None; bg = None; bold = false } in
  let expect =
    { cells =
        [| { symbol = ' '; styles = base_style }
         ; { symbol = ' '; styles = base_style }
         ; { symbol = 'x'; styles = base_style }
         ; { symbol = 'x'; styles = base_style }
        |]
    ; rows = 2
    ; cols = 2
    }
  in
  let vp = ref @@ make ~cols:2 ~rows:2 in
  set_cell 'x' ~col:0 ~row:1 ~vp;
  set_cell 'x' ~col:1 ~row:1 ~vp;
  [%eq: t] !vp expect
;;
