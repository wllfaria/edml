let buffer_id = ref 0
let pane_id = ref 0

let next_id ~id_ref =
  let id = !id_ref in
  id_ref := !id_ref + 1;
  id
;;
