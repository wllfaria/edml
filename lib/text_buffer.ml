type t =
  { text_object : Text_object.t
  ; id : int
  }
[@@deriving show { with_path = false }]

let make text_object id = { text_object; id }
