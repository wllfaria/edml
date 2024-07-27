open Effect
open Effect.Deep
open Core_unix

type _ t += Log : string -> unit t

type log_level =
  | Trace
  | Info
  | Debug
  | Warn
  | Error

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
  | Trace -> "[TRACE]"
  | Info -> "[INFO]"
  | Debug -> "[DEBUG]"
  | Warn -> "[WARN]"
  | Error -> "[ERROR]"
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

let add_metadata msg level =
  let msg = string_of_log_level level ^ " " ^ msg in
  let msg = timestamp () ^ " " ^ msg in
  let msg = msg ^ "\n" in
  msg
;;

let trace msg =
  let msg = add_metadata msg Trace in
  try_with perform (Log msg) !handler
;;

let info msg =
  let msg = add_metadata msg Info in
  try_with perform (Log msg) !handler
;;

let debug msg =
  let msg = add_metadata msg Debug in
  try_with perform (Log msg) !handler
;;

let warn msg =
  let msg = add_metadata msg Warn in
  try_with perform (Log msg) !handler
;;

let error msg =
  let msg = add_metadata msg Error in
  try_with perform (Log msg) !handler
;;

let append_to_log_file msg =
  let filename = !log_file_path in
  try
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
  with
  | _ -> ()
;;

let log msg ~level =
  match level with
  | Trace -> trace msg
  | Info -> info msg
  | Debug -> debug msg
  | Warn -> warn msg
  | Error -> error msg
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
