type log_level =
  | Trace
  | Info
  | Debug
  | Warn
  | Error
type log_mode = Async

val init : string -> log_mode -> unit
val log : string -> log_level -> unit
val trace : string -> unit
val info : string -> unit
val debug : string -> unit
val warn : string -> unit
val error : string -> unit
