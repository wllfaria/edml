open Core

type t =
  { cells : cell list
  ; cols : int
  ; rows : int
  }
[@@deriving eq, show { with_path = false }]

and cell_style =
  { fg : color option
  ; bg : color option
  ; bold : bool
  }
[@@deriving eq, show { with_path = false }]

and color = string [@@deriving eq, show]

and cell =
  { symbol : char
  ; styles : cell_style
  }
[@@deriving eq, show { with_path = false }]

and change =
  { cell : cell
  ; col : int
  ; row : int
  }
[@@deriving eq, show { with_path = false }]

let make_cell () = { symbol = ' '; styles = { fg = None; bg = None; bold = false } }

let make ~cols ~rows =
  let rec make_cells row col acc =
    if row >= rows
    then acc
    else if col >= cols
    then make_cells (row + 1) 0 acc
    else make_cells row (col + 1) (make_cell () :: acc)
  in
  let cells = List.rev @@ make_cells 0 0 [] in
  { cells; cols; rows }
;;

let normalize_col_row col row total_cols = (row * total_cols) + col

let set_cell char ~col ~row ~vp =
  let pos = normalize_col_row col row vp.cols in
  let new_cells =
    List.mapi vp.cells ~f:(fun idx cell ->
      if phys_equal idx pos then { cell with symbol = char } else cell)
  in
  { vp with cells = new_cells }
;;

let set_text text ~col ~row ~vp =
  let pos = normalize_col_row col row vp.cols in
  let new_cells =
    List.mapi vp.cells ~f:(fun idx cell ->
      if idx >= pos && idx < pos + String.length text
      then (
        let new_symbol = String.get text (idx - pos) in
        { cell with symbol = new_symbol })
      else cell)
  in
  { vp with cells = new_cells }
;;

let to_changes viewport =
  List.mapi viewport.cells ~f:(fun idx cell ->
    let row = idx / viewport.cols in
    let col = idx mod viewport.cols in
    { cell; col; row })
;;

let diff ~prev ~curr =
  let changes = ref [] in
  List.iteri prev.cells ~f:(fun idx cell ->
    let row = idx / prev.cols in
    let col = idx mod prev.cols in
    match phys_equal (List.length prev.cells) (List.length curr.cells) with
    | false -> changes := { cell; col; row } :: !changes
    | true ->
      (match phys_equal cell (List.nth_exn curr.cells idx) with
       | true -> ()
       | false -> changes := { cell = List.nth_exn curr.cells idx; col; row } :: !changes));
  List.rev !changes
;;

let pp_cells cells width =
  let height = (List.length cells + width - 1) / width in
  let grid = Array.make_matrix ~dimx:height ~dimy:width ' ' in
  List.iteri cells ~f:(fun idx cell ->
    let row = idx / width in
    let col = idx mod width in
    grid.(row).(col) <- cell.symbol);
  Array.iter grid ~f:(fun row -> printf "%s\n" (String.of_char_list (Array.to_list row)))
;;

let%test "should create correct sized viewport" =
  let expected_len = 100 in
  let result = make ~cols:10 ~rows:10 in
  phys_equal expected_len (List.length result.cells)
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
        [ sample_cell; make_cell (); sample_cell; make_cell (); sample_cell; sample_cell ]
    }
  in
  let result = diff ~prev ~curr in
  [%eq: change list] expected result
;;

let%test "should make new vp with correct text" =
  let base_style = { fg = None; bg = None; bold = false } in
  let expect =
    { cells =
        [ { symbol = ' '; styles = base_style }
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
        ]
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
        [ { symbol = ' '; styles = base_style }
        ; { symbol = ' '; styles = base_style }
        ; { symbol = 'x'; styles = base_style }
        ; { symbol = 'x'; styles = base_style }
        ]
    ; rows = 2
    ; cols = 2
    }
  in
  let vp = make ~cols:2 ~rows:2 in
  let result = set_cell 'x' ~col:0 ~row:1 ~vp in
  let result = set_cell 'x' ~col:1 ~row:1 ~vp:result in
  [%eq: t] result expect
;;
