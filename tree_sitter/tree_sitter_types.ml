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

type ts_query_error =
  | TSQueryErrorNone
  | TSQueryErrorSyntax
  | TSQueryErrorNodeType
  | TSQueryErrorField
  | TSQueryErrorCapture
  | TSQueryErrorStructure
  | TSQueryErrorLanguage
[@@deriving eq, show { with_path = false }]

type node =
  { inner : Bindings.Types.ts_node Ctypes.structure [@opaque]
  ; range : range
  }
[@@deriving show { with_path = false }]

type query_capture =
  { node : node
  ; index : int
  }
[@@deriving show { with_path = false }]

type query_match =
  { id : int
  ; pattern_index : int
  ; capture_count : int
  ; captures : query_capture list
  }
[@@deriving show { with_path = false }]

type query =
  { inner : Bindings.Types.ts_query [@opaque]
  ; capture_names : string array
  ; capture_quantifiers : int array array
  }
[@@deriving show { with_path = false }]
