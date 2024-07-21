type parser = { inner : Bindings.Types.ts_parser }

type point =
  { row : int
  ; col : int
  }
[@@deriving eq, show { with_path = false }]

type range =
  { start_point : point
  ; end_point : point
  ; start_byte : int
  ; end_byte : int
  }
[@@deriving eq, show { with_path = false }]

type input_encoding =
  | InputEncodingUTF8
  | InputEncodingUTF16
[@@deriving eq, show { with_path = false }]

type 'a input =
  { payload : 'a
  ; read : 'a -> int -> point -> int -> char
  ; encoding : input_encoding
  }
[@@deriving show { with_path = false }]
