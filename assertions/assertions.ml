exception Assertion_failure of string

let unreachable _ = failwith "reached unreachable code"
let todo () = failwith "not yet implemented"
let panic msg = failwith msg

let expect v ~msg =
  match v with
  | Some v -> v
  | None -> failwith msg
;;

let assert_true truthy msg = if not truthy then raise @@ Assertion_failure msg
