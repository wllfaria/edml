open Core
open Edml_term_lib
open Edml
open Cursor
open Viewport

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

let%test "should scroll correctly" =
  let base_style = { fg = None; bg = None; bold = false } in
  let expected_first =
    [| { symbol = 'a'; styles = base_style }
     ; { symbol = 'a'; styles = base_style }
     ; { symbol = 'b'; styles = base_style }
     ; { symbol = 'b'; styles = base_style }
     ; { symbol = 'x'; styles = base_style }
     ; { symbol = 'x'; styles = base_style }
    |]
  in
  let expected_second =
    [| { symbol = 'b'; styles = base_style }
     ; { symbol = 'b'; styles = base_style }
     ; { symbol = 'x'; styles = base_style }
     ; { symbol = 'x'; styles = base_style }
     ; { symbol = 'y'; styles = base_style }
     ; { symbol = 'y'; styles = base_style }
    |]
  in
  let expects = [| expected_first; expected_second |] in
  let text_object = Text_object.make "aaaaaa\nbbbbbb\nxxxxxx\nyyyyyy" in
  let cursor = { row = 0; col = 0; real_col = 0; offset_col = 0; offset_row = 0 } in
  let pos : position = { row = 0; col = 0; width = 2; height = 3 } in
  let vp = ref @@ make ~cols:pos.width ~rows:pos.height in
  Viewport.fill text_object cursor vp pos;
  let cells = Array.copy !vp.cells in
  let first_result = { cells; rows = 2; cols = 2 } in
  let cursor = { row = 3; col = 0; real_col = 0; offset_col = 0; offset_row = 1 } in
  Viewport.fill text_object cursor vp pos;
  let cells = Array.copy !vp.cells in
  let second_result = { cells; rows = 2; cols = 2 } in
  let results = [| first_result.cells; second_result.cells |] in
  Array.zip_exn expects results
  |> Array.for_all ~f:(fun (expect, result) ->
    Array.for_alli expect ~f:(fun idx cell -> [%eq: cell] cell result.(idx)))
;;
