type t =
  { cursor : Cursor.t ref
  ; buffer_id : int
  }
[@@deriving show { with_path = false }]

let make buffer_id = { cursor = ref (Cursor.make ()); buffer_id }
