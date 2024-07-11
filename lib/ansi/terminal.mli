type dimensions =
  { cols : int
  ; rows : int
  }

val enable_raw_mode : unit -> unit
val disable_raw_mode : unit -> unit
val clear_screen : unit -> unit
val enter_alternate_screen : unit -> unit
val leave_alternate_screen : unit -> unit
val size : unit -> dimensions
val show_dimensions : dimensions -> string
