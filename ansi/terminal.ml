open Unix

type dimensions =
  { cols : int
  ; rows : int
  }
[@@deriving eq, show { with_path = false }]

module Ffi = struct
  external size : unit -> dimensions = "edml_get_terminal_size"
  external get_winch_number : unit -> int = "edml_get_winch_number"
end

let set_resize_handler (f : unit -> unit) : unit =
  let winch_number = Ffi.get_winch_number () in
  ignore
  @@ Core.Signal.Expert.(
       signal (Core.Signal.of_caml_int winch_number) (`Handle (fun _ -> f ())))
;;

let original_mode = ref None
let tty_fd = ref None
let size () = Ffi.size ()

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

type clear_type =
  | All
  | Purge
  | FromCursorDown
  | FromCursorUp
  | CurrentLine
  | UntilNewLine
[@@deriving eq, show { with_path = false }]

type t =
  | DisableLineWrap
  | EnableLineWrap
  | EnterAlternateScreen
  | LeaveAlternateScreen
  | ScrollUp of int
  | ScrollDown of int
  | SetSize of int * int
  | SetTitle of string
  | BeginSyncUpdate
  | EndSyncUpdate
  | ClearScreen of clear_type
[@@deriving eq, show { with_path = false }]

(** Disables line wrapping. *)
let disable_line_wrap = Escape.escape "?7l"

(** Enables line wrapping. *)
let enable_line_wrap = Escape.escape "?7h"

(** A command that switches to the alternate screen buffer. *)
let enter_alternate_screen = Escape.escape "?1049h"

(** A command that switches back from the alternate screen buffer. *)
let leave_alternate_screen = Escape.escape "?1049l"

(** A command that scrolls the terminal up a given number of rows. *)
let scroll_up n = Escape.escape @@ Printf.sprintf "%dS" n

(** A command that scrolls the terminal down a given number of rows. *)
let scroll_down n = Escape.escape @@ Printf.sprintf "%dT" n

(** A command that sets the terminal buffer size (columns, rows). *)
let set_size cols rows = Escape.escape @@ Printf.sprintf "8;%d;%dt" rows cols

(** A command that sets the terminal title. *)
let set_title title = Printf.sprintf "\x1b]0;%s\x07" title

(** A command that tells the terminal to begin a synchronous update.

    Terminal emulators usually iterates through each grid cell in the visible
    screen and renders its current state. Applications that updates the screen
    at a higher frequency can experience tearing.

    When a synchronous update is enabled, the terminal will keep rendering the
    previous frame until the application is ready to render the next frame.

    Disabling synchronous update will cause the terminal to render the screen
    as soon as possible. *)
let begin_sync_update = Escape.escape "?2026h"

(** A command that tells the terminal to end a synchronous update.

    Terminal emulators usually iterates through each grid cell in the visible
    screen and renders its current state. Applications that updates the screen
    at a higher frequency can experience tearing.

    When a synchronous update is enabled, the terminal will keep rendering the
    previous frame until the application is ready to render the next frame.

    Disabling synchronous update will cause the terminal to render the screen
    as soon as possible. *)
let end_sync_update = Escape.escape "?2026l"

(** A command that clears the terminal screen buffer.
    [clear_type] specifies the type of clear to perform.
    - [All] clears the entire screen.
    - [Purge] clears the entire screen and the scrollback buffer. (history)
    - [FromCursorDown] clears from the cursor to the end of the screen.
    - [FromCursorUp] clears from the cursor to the beginning of the screen.
    - [CurrentLine] clears the current line.
    - [UntilNewLine] clears from the cursor until the new line. *)
let clear_screen clear_type =
  match clear_type with
  | All -> Escape.escape "2J"
  | Purge -> Escape.escape "3J"
  | FromCursorDown -> Escape.escape "J"
  | FromCursorUp -> Escape.escape "1J"
  | CurrentLine -> Escape.escape "2K"
  | UntilNewLine -> Escape.escape "1K"
;;

let execute command =
  match command with
  | DisableLineWrap -> disable_line_wrap
  | EnableLineWrap -> enable_line_wrap
  | EnterAlternateScreen -> enter_alternate_screen
  | LeaveAlternateScreen -> leave_alternate_screen
  | ScrollUp n -> scroll_up n
  | ScrollDown n -> scroll_down n
  | SetSize (cols, rows) -> set_size cols rows
  | SetTitle title -> set_title title
  | BeginSyncUpdate -> begin_sync_update
  | EndSyncUpdate -> end_sync_update
  | ClearScreen clear_type -> clear_screen clear_type
;;
