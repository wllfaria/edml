type t =
  { cursor : Cursor.t ref
  ; buffer : Text_buffer.t ref
  }

let make buffer = { cursor = ref (Cursor.make ()); buffer }
