open Core
open Types

type t =
  { cells : cell array
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

and color = Color of string [@@deriving eq, show]

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
  let cells = Array.create ~len:(cols * rows) (make_cell ()) in
  { cells; cols; rows }
;;

let normalize_col_row col row total_cols = (row * total_cols) + col

let set_cell char ~col ~row ~vp =
  let pos = normalize_col_row col row vp.cols in
  let new_cells =
    Array.mapi vp.cells ~f:(fun idx cell ->
      if phys_equal idx pos then { cell with symbol = char } else cell)
  in
  { vp with cells = new_cells }
;;

let set_text text ~col ~row ~vp =
  let pos = normalize_col_row col row vp.cols in
  let new_cells =
    Array.mapi vp.cells ~f:(fun idx cell ->
      if idx >= pos && idx < pos + String.length text
      then (
        let new_symbol = String.get text (idx - pos) in
        { cell with symbol = new_symbol })
      else cell)
  in
  { vp with cells = new_cells }
;;

let to_changes viewport =
  Array.mapi viewport.cells ~f:(fun idx cell ->
    let row = idx / viewport.cols in
    let col = idx mod viewport.cols in
    { cell; col; row })
;;

let diff ~prev ~curr =
  if Array.length prev.cells <> Array.length curr.cells
  then failwith "Viewports have different sizes"
  else (
    let changes = ref [] in
    Array.iteri prev.cells ~f:(fun idx cell ->
      let row = idx / prev.cols in
      let col = idx mod prev.cols in
      let other = curr.cells.(idx) in
      if not (phys_equal cell other)
      then changes := { cell = other; col; row } :: !changes);
    List.rev !changes)
;;

let fill text_object viewport (position : position) =
  let open Text_object in
  List.foldi text_object.content ~init:viewport ~f:(fun row vp line ->
    let row = position.row + row in
    if row < vp.rows && row < position.row + position.height
    then
      String.foldi line ~init:vp ~f:(fun col vp char ->
        let col = col + position.col in
        if col < vp.cols && col < position.col + position.width
        then set_cell char ~col ~row ~vp
        else vp)
    else vp)
;;

let pp_cells cells width =
  let height = (Array.length cells + width - 1) / width in
  let grid = Array.make_matrix ~dimx:height ~dimy:width ' ' in
  Array.iteri cells ~f:(fun idx cell ->
    let row = idx / width in
    let col = idx mod width in
    grid.(row).(col) <- cell.symbol);
  Array.iter grid ~f:(fun row -> printf "%s\n" (String.of_char_list (Array.to_list row)))
;;
