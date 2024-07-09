type color = string [@@deriving show]

type cell_style =
  { fg : color option
  ; bg : color option
  ; bold : bool
  }
[@@deriving show { with_path = false }]

type cell =
  { symbol : char
  ; styles : cell_style
  }
[@@deriving show { with_path = false }]

type buffer = { cells : cell list } [@@deriving show { with_path = false }]

let make_cell () = { symbol = ' '; styles = { fg = None; bg = None; bold = false } }

let make_buffer cols rows =
  let total_cells = cols * rows in
  let rec make_cells list =
    match List.length list < total_cells with
    | true -> make_cells (make_cell () :: list)
    | false -> list
  in
  let cells = make_cells [] in
  { cells }
;;

let%test "should create correct sized buffer" =
  let expected_len = 100 in
  let result = make_buffer 10 10 in
  expected_len == List.length result.cells
;;
