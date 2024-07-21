exception Assertion_failure of string

let unreachable _ = failwith "reached unreachable code"
let todo () = failwith "not yet implemented"
let panic msg = failwith msg
let assert_true truthy msg = if not truthy then raise @@ Assertion_failure msg
