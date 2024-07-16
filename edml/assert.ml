exception Assertion_failure of string

let unreachable _ = failwith "reached unreachable code"
let todo _ = failwith "not yet implemented"
let assert_true truthy msg = if not truthy then raise @@ Assertion_failure msg
