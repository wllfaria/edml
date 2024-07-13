type position =
  { row : int
  ; col : int
  ; width : int
  ; height : int
  }
[@@deriving show { with_path = false }]
