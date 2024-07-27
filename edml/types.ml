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

module IntTuple = struct
  module T = struct
    type t = int * int [@@deriving sexp, compare, hash, eq, show { with_path = false }]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)
end

type match_map = (IntTuple.t, string) Hashtbl.t
