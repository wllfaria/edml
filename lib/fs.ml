open Core

let file_exists path =
  match Core_unix.access path [ `Exists ] with
  | Ok _ -> true
  | Error _ -> false
;;

let maybe_read_file path =
  if file_exists path
  then (
    let fd = Core_unix.openfile path ~mode:[ Core_unix.O_RDONLY ] in
    let file_size = (Core_unix.fstat fd).st_size in
    let buffer = Bytes.create @@ Int64.to_int_exn file_size in
    let _ = Core_unix.read fd ~buf:buffer in
    Some (Bytes.to_string buffer))
  else None
;;
