open Core

type t =
  | SetForegroundColor of Color.t
  | SetBackgroundColor of Color.t
  (* | SetAttribute of Style.t *)
  (* | SetAttributes of Style.t list *)
  | Cursor of Cursor.t
  | Terminal of Terminal.t
  | Print of string
(* | PrintStyled of Style.styled *)
[@@deriving show { with_path = false }]

let format_command cmd =
  match cmd with
  | SetForegroundColor color ->
    Escape.escape @@ Escape.foreground ^ ";" ^ Color.ansii_of_color color ^ "m"
  | SetBackgroundColor color ->
    Escape.escape @@ Escape.background ^ ";" ^ Color.ansii_of_color color ^ "m"
  | Print s -> s
  | Cursor c -> Cursor.execute c
  | Terminal c -> Terminal.execute c
;;

let command_queue : t list ref = ref []

let queue cmds =
  List.iter cmds ~f:(fun cmd ->
    (* Logger.info @@ show cmd; *)
    command_queue := cmd :: !command_queue)
;;

let execute cmds =
  List.iter ~f:(fun cmd -> print_string @@ format_command cmd) cmds;
  Out_channel.flush stdout
;;

let flush () =
  List.iter (List.rev !command_queue) ~f:(fun cmd -> print_string @@ format_command cmd);
  command_queue := [];
  Out_channel.flush stdout
;;

let clear_queue () = command_queue := []
