exception Assertion_failure of string

let assert_true truthy message = if not truthy then raise @@ Assertion_failure message
