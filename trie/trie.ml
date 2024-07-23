open Core

type 'a node =
  { final : bool
  ; value : char
  ; data : 'a option
  ; childrens : 'a node list
  }
[@@deriving eq, show { with_path = false }]

type 'a query_result =
  { continues : bool
  ; data : 'a option
  }
[@@deriving eq, show { with_path = false }]

let make ~final ~value ~data = { final; value; childrens = []; data }
let empty _ = { final = false; value = ' '; childrens = []; data = None }
let with_children ~final ~value ~data ~childrens = { final; value; childrens; data }

let rec add_word ~node ~word ~data =
  if String.length word = 0
  then node
  else (
    let final = String.length word = 1 in
    let value = String.get word 0 in
    let rest = String.sub word ~pos:1 ~len:(String.length word - 1) in
    let existing = List.find node.childrens ~f:(fun node -> phys_same node.value value) in
    match existing with
    | Some inner ->
      let updated_inner = add_word ~node:inner ~word:rest ~data in
      let new_childrens =
        List.map node.childrens ~f:(fun node ->
          if phys_same node.value value then updated_inner else node)
      in
      { node with childrens = new_childrens }
    | None ->
      let new_node =
        if final then make ~final ~value ~data else make ~final ~value ~data:None
      in
      let new_childrens = add_word ~node:new_node ~word:rest ~data :: node.childrens in
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
       | true -> Some { data = inner.data; continues = List.length inner.childrens > 0 }
       | false -> find_word ~node:inner ~word:rest)
    | None -> None)
;;

let%test "add word on empty trie" =
  let c = with_children in
  let expected = make ~final:true ~value:'i' ~data:None in
  let expected = c ~final:false ~value:'h' ~data:None ~childrens:[ expected ] in
  let expected = c ~final:false ~value:' ' ~data:None ~childrens:[ expected ] in
  let root = empty () in
  let result = add_word ~node:root ~word:"hi" ~data:None in
  [%eq: string node] expected result
;;

let%test "add word on existing trie" =
  let c = with_children in
  let a = make ~final:true ~value:'i' ~data:None in
  let a = c ~final:false ~value:'h' ~childrens:[ a ] ~data:None in
  let b = make ~final:true ~value:'o' ~data:None in
  let b = c ~final:false ~value:'y' ~childrens:[ b ] ~data:None in
  let expected = c ~final:false ~value:' ' ~childrens:[ b; a ] ~data:None in
  let root = empty () in
  let root = { root with childrens = [ a ] } in
  let result = add_word ~node:root ~word:"yo" ~data:None in
  [%eq: string node] expected result
;;

let%test "add word that share letters" =
  let c = with_children in
  let expected = make ~final:true ~value:'!' ~data:None in
  let expected = c ~final:true ~value:'i' ~childrens:[ expected ] ~data:None in
  let expected = c ~final:false ~value:'h' ~childrens:[ expected ] ~data:None in
  let expected = c ~final:false ~value:' ' ~childrens:[ expected ] ~data:None in
  let root = make ~final:true ~value:'i' ~data:None in
  let root = c ~final:false ~value:'h' ~childrens:[ root ] ~data:None in
  let root = { (empty ()) with childrens = [ root ] } in
  let result = add_word ~node:root ~word:"hi!" ~data:None in
  [%eq: string node] expected result
;;

let%test "find existing word at end of branch" =
  let c = with_children in
  let expected = Some { continues = false; data = None } in
  let root = make ~final:true ~value:'!' ~data:None in
  let root = c ~final:true ~value:'i' ~childrens:[ root ] ~data:None in
  let root = c ~final:false ~value:'h' ~childrens:[ root ] ~data:None in
  let root = c ~final:false ~value:' ' ~childrens:[ root ] ~data:None in
  let result = find_word ~node:root ~word:"hi!" in
  [%eq: string query_result option] expected result
;;

let%test "find non existing word" =
  let c = with_children in
  let root = make ~final:true ~value:'!' ~data:None in
  let root = c ~final:true ~value:'i' ~childrens:[ root ] ~data:None in
  let root = c ~final:false ~value:'h' ~childrens:[ root ] ~data:None in
  let root = c ~final:false ~value:' ' ~childrens:[ root ] ~data:None in
  let result = find_word ~node:root ~word:"nop" in
  [%eq: string query_result option] None result
;;

let%test "find existing word that continues" =
  let c = with_children in
  let expected = Some { continues = true; data = None } in
  let root = make ~final:true ~value:'!' ~data:None in
  let root = c ~final:true ~value:'i' ~childrens:[ root ] ~data:None in
  let root = c ~final:false ~value:'h' ~childrens:[ root ] ~data:None in
  let root = c ~final:false ~value:' ' ~childrens:[ root ] ~data:None in
  let result = find_word ~node:root ~word:"hi" in
  [%eq: string query_result option] expected result
;;
