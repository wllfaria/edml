open Escape

type cursor_style =
  | Default
  | BlinkingBlock
  | SteadyBlock
  | BlinkingUnderscore
  | SteadyUnderscore
  | BlinkingBar
  | SteadyBar
[@@deriving eq, show { with_path = false }]

type t =
  | Hide
  | Show
  | NextLine
  | SavePosition
  | PreviousLine
  | EnableBlinking
  | RestorePosition
  | DisableBlinking
  | MoveTo of int * int
  | ToColumn of int
  | ToRow of int
  | Up of int
  | Right of int
  | Down of int
  | Left of int
  | SetStyle of cursor_style
[@@deriving eq, show { with_path = false }]

let move_to x y = MoveTo (x, y)
let next_line () = NextLine
let previous_line () = PreviousLine
let to_column x = ToColumn x
let to_row y = ToRow y
let up n = Up n
let right n = Right n
let down n = Down n
let left n = Left n
let save_position () = SavePosition
let restore_position () = RestorePosition
let hide () = Hide
let show () = Show
let enable_blinking () = EnableBlinking
let disable_blinking () = DisableBlinking
let set_style style = SetStyle style

let execute command =
  match command with
  | MoveTo (x, y) -> escape (Printf.sprintf "%d;%dH" (y + 1) (x + 1))
  | NextLine -> escape "1E"
  | PreviousLine -> escape "1F"
  | ToColumn x -> escape (Printf.sprintf "%dG" (x + 1))
  | ToRow y -> escape (Printf.sprintf "%dd" (y + 1))
  | Up n -> escape (Printf.sprintf "%dA" n)
  | Right n -> escape (Printf.sprintf "%dC" n)
  | Down n -> escape (Printf.sprintf "%dB" n)
  | Left n -> escape (Printf.sprintf "%dD" n)
  | SavePosition -> escape "7"
  | RestorePosition -> escape "8"
  | Hide -> escape "?25l"
  | Show -> escape "?25h"
  | EnableBlinking -> escape "?12h"
  | DisableBlinking -> escape "?12l"
  | SetStyle style ->
    (match style with
     | Default -> escape "0 q"
     | BlinkingBlock -> escape "1 q"
     | SteadyBlock -> escape "2 q"
     | BlinkingUnderscore -> escape "3 q"
     | SteadyUnderscore -> escape "4 q"
     | BlinkingBar -> escape "5 q"
     | SteadyBar -> escape "6 q")
;;
