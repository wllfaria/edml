open Unix

let original_mode = ref None
let tty_fd = ref None

let is_tty fd =
  try
    let _ = Unix.tcgetattr fd in
    true
  with
  | Unix_error (EINVAL, _, _) -> false
;;

let open_tty () =
  if is_tty Unix.stdin
  then Unix.stdin, false
  else (
    let fd = Unix.openfile "/dev/tty" [ O_RDWR ] 0 in
    fd, true)
;;

let enable_raw_mode () =
  let fd, close_on_drop = open_tty () in
  if close_on_drop then tty_fd := Some fd;
  match !original_mode with
  | Some _ -> ()
  | None ->
    let curr_mode = Unix.tcgetattr fd in
    original_mode := Some curr_mode;
    let raw_ios =
      { curr_mode with
        c_icanon = false
      ; c_echo = false
      ; c_ignbrk = true
      ; c_brkint = false
      ; c_parmrk = false
      ; c_istrip = false
      ; c_inlcr = false
      ; c_igncr = false
      ; c_icrnl = false
      ; c_opost = false
      }
    in
    Unix.tcsetattr fd TCSANOW raw_ios
;;

let disable_raw_mode () =
  let fd, close_fd =
    match !tty_fd with
    | Some fd -> fd, true
    | None -> Unix.stdin, false
  in
  match !original_mode with
  | Some mode -> Unix.tcsetattr fd TCSANOW mode
  | None ->
    ();
    if close_fd then Unix.close fd
;;

let clear_screen () = Printf.printf "\x1b[2J"
let enter_alternate_screen () = Printf.printf "\x1b[?1049h"
let leave_alternate_screen () = Printf.printf "\x1b[?1049l"

type dimensions =
  { cols : int
  ; rows : int
  }
[@@deriving show { with_path = false }]

external size : unit -> dimensions = "edml_get_terminal_size"

let%test "help me" =
  let result = size () in
  print_endline @@ show_dimensions result;
  false
;;
