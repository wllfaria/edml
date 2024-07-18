open Core

type position =
  { row : int
  ; col : int
  ; width : int
  ; height : int
  }
[@@deriving show { with_path = false }]

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

let make_cell_with_symbol symbol =
  { symbol; styles = { fg = None; bg = None; bold = false } }
;;

let make ~cols ~rows =
  let cells = Array.create ~len:(cols * rows) (make_cell ()) in
  { cells; cols; rows }
;;

let normalize_col_row col row total_cols = (row * total_cols) + col

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

let set_cell char ~col ~row ~vp =
  let pos = normalize_col_row col row !vp.cols in
  !vp.cells.(pos) <- make_cell_with_symbol char
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
      if not ([%eq: cell] cell other)
      then changes := { cell = other; col; row } :: !changes);
    List.rev !changes)
;;

let fill text_object cursor viewport (position : position) =
  let open Text_object in
  let open Cursor in
  let len = min !viewport.rows (text_object.lines - cursor.offset_row) in
  let content_onscreen = List.sub text_object.content ~pos:cursor.offset_row ~len in
  for row = 0 to len - 1 do
    let line = List.nth_exn content_onscreen row in
    let max_col = min (position.width - position.col) (String.length line) in
    for col = 0 to max_col - 1 do
      let char = String.get line col in
      set_cell char ~col:(position.col + col) ~row:(position.row + row) ~vp:viewport
    done;
    for col = max_col to position.width - position.col - 1 do
      set_cell ' ' ~col:(position.col + col) ~row:(position.row + row) ~vp:viewport
    done
  done
;;

let copy vp = { vp with cells = Array.copy vp.cells }

let pp_cells cells width =
  let height = (Array.length cells + width - 1) / width in
  let grid = Array.make_matrix ~dimx:height ~dimy:width ' ' in
  Array.iteri cells ~f:(fun idx cell ->
    let row = idx / width in
    let col = idx mod width in
    grid.(row).(col) <- cell.symbol);
  Array.iter grid ~f:(fun row -> printf "%s\n" (String.of_char_list (Array.to_list row)))
;;
