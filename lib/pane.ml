type pane =
  { cursor : Cursor.cursor
  ; buffer : Buffer.buffer
  }

let make buffer = { cursor = Cursor.make (); buffer }
