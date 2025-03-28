type pane =
  { cursor : Cursor.cursor ref
  ; id : int
  ; buffer_id : int
  }
[@@deriving eq, show { with_path = false }]

let make ~buffer_id ~id = { cursor = ref (Cursor.make ()); buffer_id; id }
