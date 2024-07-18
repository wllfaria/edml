open Core
open Assert
open Types

type action =
  | TypeChar of char
  | DeleteLine
  | DeleteCurrChar
  | DeletePrevChar
  | DeleteUntilEOL
[@@deriving eq, show { with_path = false }]

type t =
  { content : string list
  ; lines : int
  }
[@@deriving eq, show { with_path = false }]

let make source =
  let content = String.split_lines source in
  let lines = List.length content in
  { content; lines }
;;

let insert_char ~char ~text_object ~anchor =
  assert_true
    (text_object.lines > anchor.row)
    (Format.sprintf "editing out of bounds on line: %d" anchor.row);
  let line = List.nth_exn text_object.content anchor.row in
  assert_true
    (String.length line > anchor.col)
    (Format.sprintf "editing out of bounds on line %d column %d" anchor.row anchor.col);
  let new_line =
    let line_len = String.length line in
    let prefix = String.sub line ~pos:0 ~len:anchor.col in
    let suffix = String.sub line ~pos:anchor.col ~len:(line_len - anchor.col) in
    prefix ^ Char.to_string char ^ suffix
  in
  let content =
    let before = List.take text_object.content anchor.row in
    let after = List.drop text_object.content (anchor.row + 1) in
    before @ [ new_line ] @ after
  in
  { text_object with content }
;;

let delete_line ~anchor ~text_object =
  let content =
    let before = List.take text_object.content anchor.row in
    let after = List.drop text_object.content (anchor.row + 1) in
    before @ after
  in
  { text_object with content }
;;

let delete_until_eol ~anchor ~text_object =
  let line = List.nth_exn text_object.content anchor.row in
  let new_line = String.sub line ~pos:0 ~len:anchor.col in
  let content =
    let before = List.take text_object.content anchor.row in
    let after = List.drop text_object.content (anchor.row + 1) in
    before @ [ new_line ] @ after
  in
  { text_object with content }
;;

let delete_curr_char ~anchor ~text_object =
  let line = List.nth_exn text_object.content anchor.row in
  let new_line =
    let line_len = String.length line in
    let prefix = String.sub line ~pos:0 ~len:anchor.col in
    let suffix = String.sub line ~pos:(anchor.col + 1) ~len:(line_len - anchor.col - 1) in
    prefix ^ suffix
  in
  let content =
    let before = List.take text_object.content anchor.row in
    let after = List.drop text_object.content (anchor.row + 1) in
    before @ [ new_line ] @ after
  in
  { text_object with content }
;;

let delete_prev_char ~anchor ~text_object =
  let line = List.nth_exn text_object.content anchor.row in
  let new_line =
    let line_len = String.length line in
    let prefix = String.sub line ~pos:0 ~len:(anchor.col - 1) in
    let suffix = String.sub line ~pos:anchor.col ~len:(line_len - anchor.col - 1) in
    prefix ^ suffix
  in
  let content =
    let before = List.take text_object.content anchor.row in
    let after = List.drop text_object.content (anchor.row + 1) in
    before @ [ new_line ] @ after
  in
  { text_object with content }
;;

let handle_action ~text_object ~action ~anchor =
  match action with
  | TypeChar char -> insert_char ~char ~text_object ~anchor
  | DeleteLine -> delete_line ~anchor ~text_object
  | DeleteCurrChar -> delete_curr_char ~anchor ~text_object
  | DeletePrevChar -> delete_prev_char ~anchor ~text_object
  | DeleteUntilEOL -> delete_until_eol ~anchor ~text_object
;;
