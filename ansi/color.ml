open Core

type t =
  | Black
  | DarkGrey
  | Red
  | DarkRed
  | Green
  | DarkGreen
  | Yellow
  | DarkYellow
  | Blue
  | DarkBlue
  | Magenta
  | DarkMagenta
  | Cyan
  | DarkCyan
  | White
  | Grey
  | Reset
  | ResetFg
  | ResetBg
  | Rgb of int * int * int
[@@deriving eq, show { with_path = false }]

exception Invalid_hex_color of string
exception Invalid_color_format of string
exception Invalid_color_range of string

let invalid_hex hex = raise @@ Invalid_hex_color ("invalid hex color provided: " ^ hex)

let hex_to_u8 part ~hex =
  try int_of_string ("0x" ^ part) with
  | _ -> invalid_hex hex
;;

let color_of_hex hex =
  if not @@ phys_equal (String.length hex) 7 then invalid_hex hex;
  if not @@ phys_equal (String.get hex 0) '#' then invalid_hex hex;
  let hex = String.sub hex ~pos:1 ~len:6 in
  let r = hex_to_u8 ~hex @@ String.sub hex ~pos:0 ~len:2 in
  let g = hex_to_u8 ~hex @@ String.sub hex ~pos:2 ~len:2 in
  let b = hex_to_u8 ~hex @@ String.sub hex ~pos:4 ~len:2 in
  Rgb (r, g, b)
;;

let replace_comma s = String.filter s ~f:(fun c -> not @@ phys_same c ',')
let within_rgb_bounds colors = List.for_all colors ~f:(fun c -> c >= 0 && c <= 255)

let color_of_rgb s =
  let len = String.length s in
  let prefix = 4 in
  let sub = String.sub s ~pos:prefix ~len:(len - prefix - 1) in
  match String.split ~on:',' sub with
  | [ r; g; b ] ->
    (try
       let r = int_of_string @@ replace_comma r in
       let g = int_of_string @@ replace_comma g in
       let b = int_of_string @@ replace_comma b in
       if within_rgb_bounds [ r; g; b ]
       then Rgb (r, g, b)
       else raise (Invalid_color_range s)
     with
     | _ -> raise (Invalid_color_format s))
  | _ -> raise (Invalid_color_format s)
;;

let color_of_ansii s =
  let sub = String.sub s ~pos:2 ~len:(String.length s - 2) in
  match String.get sub 0 with
  | '5' ->
    let slices = String.split ~on:';' s in
    let c = int_of_string @@ List.nth_exn slices 1 in
    (match c with
     | 0 -> Black
     | 1 -> DarkRed
     | 2 -> DarkGreen
     | 3 -> DarkYellow
     | 4 -> DarkBlue
     | 5 -> DarkMagenta
     | 6 -> DarkCyan
     | 7 -> Grey
     | 8 -> DarkGrey
     | 9 -> Red
     | 10 -> Green
     | 11 -> Yellow
     | 12 -> Blue
     | 13 -> Magenta
     | 14 -> Cyan
     | 15 -> White
     | _ -> raise (Invalid_color_format sub))
  | '2' ->
    let slices = String.split ~on:';' s in
    let r = int_of_string @@ List.nth_exn slices 1 in
    let g = int_of_string @@ List.nth_exn slices 2 in
    let b = int_of_string @@ List.nth_exn slices 3 in
    Rgb (r, g, b)
  | _ -> raise (Invalid_color_format sub)
;;

let color_of_string s =
  let s = String.lowercase s in
  match s with
  | "black" -> Black
  | "dark_grey" -> DarkGrey
  | "red" -> Red
  | "dark_red" -> DarkRed
  | "green" -> Green
  | "dark_green" -> DarkGreen
  | "yellow" -> Yellow
  | "dark_yellow" -> DarkYellow
  | "blue" -> Blue
  | "dark_blue" -> DarkBlue
  | "magenta" -> Magenta
  | "dark_magenta" -> DarkMagenta
  | "cyan" -> Cyan
  | "dark_cyan" -> DarkCyan
  | "white" -> White
  | "grey" -> Grey
  | "reset" -> Reset
  | "resetbg" -> ResetBg
  | "resetfg" -> ResetFg
  | s when phys_same (String.get s 0) '#' -> color_of_hex s
  | s when phys_same (String.sub ~pos:0 ~len:4) "rgb(" -> color_of_rgb s
  | s when phys_same (String.sub s ~pos:0 ~len:2) "\x1b[" -> color_of_ansii s
  | _ -> raise (Invalid_color_format s)
;;

let ansii_of_color c =
  match c with
  | Black -> "5;0"
  | DarkRed -> "5;1"
  | DarkGreen -> "5;2"
  | DarkYellow -> "5;3"
  | DarkBlue -> "5;4"
  | DarkMagenta -> "5;5"
  | DarkCyan -> "5;6"
  | Grey -> "5;7"
  | DarkGrey -> "5;8"
  | Red -> "5;9"
  | Green -> "5;10"
  | Yellow -> "5;11"
  | Blue -> "5;12"
  | Magenta -> "5;13"
  | Cyan -> "5;14"
  | White -> "5;15"
  | Reset -> "0"
  | ResetFg -> "39"
  | ResetBg -> "49"
  | Rgb (r, g, b) -> Format.sprintf "2;%d;%d;%d" r g b
;;

let from s = color_of_string s
