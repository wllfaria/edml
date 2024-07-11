type t =
  { cursor : Cursor.t ref
  ; buffer : Text_buffer.t ref
  ; position : position
  }

and position =
  { col : int
  ; row : int
  ; width : int
  ; height : int
  }

let make buffer position = { cursor = ref (Cursor.make ()); buffer; position }
