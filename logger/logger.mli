type log_level = Info
type log_mode = Async

val init : string -> log_mode -> unit
val log : string -> log_level -> unit
