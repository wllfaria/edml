open Effect
open Effect.Deep
open Core_unix

type _ t += Log : string -> unit t
type log_level = Info

let dummy_handler =
  { effc =
      (fun (type a) (eff : a t) ->
        match eff with
        | Log _ -> Some (fun (k : (a, _) continuation) -> continue k ())
        | _ -> None)
  }
;;

let log_file_path = ref ""
let handler : 'a effect_handler ref = ref dummy_handler

let string_of_log_level level =
  match level with
  | Info -> "[INFO]"
;;

let timestamp () =
  let tm = localtime (time ()) in
  Printf.sprintf
    "[%04d-%02d-%02d %02d:%02d:%02d]"
    (tm.tm_year + 1900)
    (tm.tm_mon + 1)
    tm.tm_mday
    tm.tm_hour
    tm.tm_min
    tm.tm_sec
;;

type log_mode = Async

let info msg =
  let msg = string_of_log_level Info ^ " " ^ msg in
  let msg = timestamp () ^ " " ^ msg in
  let msg = msg ^ "\n" in
  perform @@ Log msg
;;

let append_to_log_file msg =
  let filename = !log_file_path in
  let fd = openfile filename ~mode:[ O_WRONLY; O_APPEND; O_CREAT ] in
  try
    let len = String.length msg in
    let rec write_loop offset =
      if offset < len
      then (
        let written =
          write fd ~buf:(Bytes.of_string msg) ~pos:offset ~len:(len - offset)
        in
        write_loop (offset + written))
      else ()
    in
    write_loop 0;
    close fd
  with
  | _ -> close fd
;;

let log msg level =
  match level with
  | Info -> try_with info msg !handler
;;

let async_handler =
  { effc =
      (fun (type a) (eff : a t) ->
        match eff with
        | Log msg ->
          let _ = Thread.create (fun msg -> append_to_log_file msg) msg in
          Some (fun (k : (a, _) continuation) -> continue k ())
        | _ -> None)
  }
;;

let init path mode =
  log_file_path := path;
  match mode with
  | Async -> handler := async_handler
;;
