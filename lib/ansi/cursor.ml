let escape = Printf.sprintf "\x1b%s"
let move_to col row = Printf.printf "%s" @@ escape @@ Printf.sprintf "[%d;%dH" row col
let hide () = Printf.printf "\x1b[?25l"
let show () = Printf.printf "\x1b[?25h"
