open Core

let file_exists path =
  match Core_unix.access path [ `Exists ] with
  | Ok _ -> true
  | Error _ -> false
;;

let close_fd fd = Core_unix.close fd

let maybe_read_file path =
  if file_exists path
  then (
    let fd = Core_unix.openfile path ~mode:[ Core_unix.O_RDONLY ] in
    let file_size = (Core_unix.fstat fd).st_size in
    let buffer = Bytes.create @@ Int64.to_int_exn file_size in
    (try
       let _ = Core_unix.read fd ~buf:buffer in
       ()
     with
     | Core_unix.Unix_error _ -> close_fd fd);
    close_fd fd;
    Some (Bytes.to_string buffer))
  else None
;;

let join_paths parts =
  match List.length parts with
  | 0 -> ""
  | 1 -> List.nth_exn parts 0
  | _ ->
    List.sub parts ~pos:1 ~len:(List.length parts - 1)
    |> List.fold ~init:(List.nth_exn parts 0) ~f:(fun acc part -> acc ^ "/" ^ part)
;;
