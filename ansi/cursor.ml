open Core

let escape = Printf.sprintf "\x1b%s"

let move_to ~col ~row =
  Fmt.pr "%s" @@ escape @@ Printf.sprintf "[%d;%dH" (row + 1) (col + 1)
;;

let hide () = Fmt.pr "\x1b[?25l"
let show () = Fmt.pr "\x1b[?25h"
