open Core

type language_id =
  | Ocaml
  | Javascript
  | PlainText
[@@deriving sexp, compare, hash, eq, show { with_path = false }]

type anchor =
  { col : int
  ; row : int
  }

type mode =
  | Normal
  | Insert
[@@deriving eq, show { with_path = false }]

type match_map = (int, (int * int * string) list) Hashtbl.t
