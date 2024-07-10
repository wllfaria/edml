let read_file path =
  let input_channel = open_in path in
  let rec read_lines channel lines =
    try
      let line = input_line channel in
      read_lines channel (line :: lines)
    with
    | End_of_file -> List.rev lines
  in
  let lines = read_lines input_channel [] in
  close_in input_channel;
  String.concat "\n" lines
;;
