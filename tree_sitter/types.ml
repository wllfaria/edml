type point =
  { row : int
  ; col : int
  }

type range =
  { start_point : point
  ; end_point : point
  ; start_byte : int
  ; end_byte : int
  }

type input_encoding =
  | InputEncodingUTF8
  | InputEncodingUTF16

type 'a input =
  { payload : 'a
  ; read : 'a -> int -> point -> int -> char
  ; encoding : input_encoding
  }
