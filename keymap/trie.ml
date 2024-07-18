open Core

type node =
  { final : bool
  ; value : char
  ; action : Types.action option
  ; childrens : node list
  }
[@@deriving eq, show { with_path = false }]

type query_result =
  { continues : bool
  ; action : Types.action option
  }
[@@deriving eq, show { with_path = false }]

let make ~final ~value ~action = { final; value; childrens = []; action }
let empty _ = { final = false; value = ' '; childrens = []; action = None }
let with_children ~final ~value ~action ~childrens = { final; value; childrens; action }

let rec add_word ~node ~word ~action =
  if String.length word = 0
  then node
  else (
    let final = String.length word = 1 in
    let value = String.get word 0 in
    let rest = String.sub word ~pos:1 ~len:(String.length word - 1) in
    let existing = List.find node.childrens ~f:(fun node -> phys_same node.value value) in
    match existing with
    | Some inner ->
      let updated_inner = add_word ~node:inner ~word:rest ~action in
      let new_childrens =
        List.map node.childrens ~f:(fun node ->
          if phys_same node.value value then updated_inner else node)
      in
      { node with childrens = new_childrens }
    | None ->
      let new_node =
        if final then make ~final ~value ~action else make ~final ~value ~action:None
      in
      let new_childrens = add_word ~node:new_node ~word:rest ~action :: node.childrens in
      { node with childrens = new_childrens })
;;

let rec find_word ~node ~word =
  if String.length word = 0
  then None
  else (
    let is_last = String.length word = 1 in
    let value = String.get word 0 in
    let rest = String.sub word ~pos:1 ~len:(String.length word - 1) in
    let existing = List.find node.childrens ~f:(fun node -> phys_same node.value value) in
    match existing with
    | Some inner ->
      (match is_last with
       | true ->
         Some { action = inner.action; continues = List.length inner.childrens > 0 }
       | false -> find_word ~node:inner ~word:rest)
    | None -> None)
;;

let%test "add word on empty trie" =
  let c = with_children in
  let expected = make ~final:true ~value:'i' ~action:None in
  let expected = c ~final:false ~value:'h' ~action:None ~childrens:[ expected ] in
  let expected = c ~final:false ~value:' ' ~action:None ~childrens:[ expected ] in
  let root = empty () in
  let result = add_word ~node:root ~word:"hi" ~action:None in
  [%eq: node] expected result
;;

let%test "add word on existing trie" =
  let c = with_children in
  let a = make ~final:true ~value:'i' ~action:None in
  let a = c ~final:false ~value:'h' ~childrens:[ a ] ~action:None in
  let b = make ~final:true ~value:'o' ~action:None in
  let b = c ~final:false ~value:'y' ~childrens:[ b ] ~action:None in
  let expected = c ~final:false ~value:' ' ~childrens:[ b; a ] ~action:None in
  let root = empty () in
  let root = { root with childrens = [ a ] } in
  let result = add_word ~node:root ~word:"yo" ~action:None in
  [%eq: node] expected result
;;

let%test "add word that share letters" =
  let c = with_children in
  let expected = make ~final:true ~value:'!' ~action:None in
  let expected = c ~final:true ~value:'i' ~childrens:[ expected ] ~action:None in
  let expected = c ~final:false ~value:'h' ~childrens:[ expected ] ~action:None in
  let expected = c ~final:false ~value:' ' ~childrens:[ expected ] ~action:None in
  let root = make ~final:true ~value:'i' ~action:None in
  let root = c ~final:false ~value:'h' ~childrens:[ root ] ~action:None in
  let root = { (empty ()) with childrens = [ root ] } in
  let result = add_word ~node:root ~word:"hi!" ~action:None in
  [%eq: node] expected result
;;

let%test "find existing word at end of branch" =
  let c = with_children in
  let expected = Some { continues = false; action = None } in
  let root = make ~final:true ~value:'!' ~action:None in
  let root = c ~final:true ~value:'i' ~childrens:[ root ] ~action:None in
  let root = c ~final:false ~value:'h' ~childrens:[ root ] ~action:None in
  let root = c ~final:false ~value:' ' ~childrens:[ root ] ~action:None in
  let result = find_word ~node:root ~word:"hi!" in
  [%eq: query_result option] expected result
;;

let%test "find non existing word" =
  let c = with_children in
  let root = make ~final:true ~value:'!' ~action:None in
  let root = c ~final:true ~value:'i' ~childrens:[ root ] ~action:None in
  let root = c ~final:false ~value:'h' ~childrens:[ root ] ~action:None in
  let root = c ~final:false ~value:' ' ~childrens:[ root ] ~action:None in
  let result = find_word ~node:root ~word:"nop" in
  (match result with
   | Some result -> Fmt.pr "%a@." pp_query_result result
   | None -> ());
  [%eq: query_result option] None result
;;

let%test "find existing word that continues" =
  let c = with_children in
  let expected = Some { continues = true; action = None } in
  let root = make ~final:true ~value:'!' ~action:None in
  let root = c ~final:true ~value:'i' ~childrens:[ root ] ~action:None in
  let root = c ~final:false ~value:'h' ~childrens:[ root ] ~action:None in
  let root = c ~final:false ~value:' ' ~childrens:[ root ] ~action:None in
  let result = find_word ~node:root ~word:"hi" in
  [%eq: query_result option] expected result
;;
