let parsers : Tree_sitter.ts_parser list ref = ref []

type editor =
  { tabs : tab list
  ; viewport : Viewport.t ref
  ; mode : mode
  ; buffers : Text_buffer.text_buffer list
  ; active_tab : int
  }

and mode =
  | Normal
  | Insert
[@@deriving eq, show { with_path = false }]

and tab =
  { panes : pane_tree
  ; active_pane : int
  }

and pane_tree =
  | Single of Pane.t
  | Split of pane_branch
[@@deriving eq, show { with_path = false }]

and pane_branch =
  { direction : direction
  ; ratios : float list
  ; panes : pane_tree list
  }
[@@deriving eq, show { with_path = false }]

and direction =
  | Horizontal
  | Vertical
[@@deriving eq, show { with_path = false }]

type t = editor

let make ~pane ~active_pane ~buffer ~viewport =
  let tab = { panes = pane; active_pane } in
  { buffers = [ buffer ]; tabs = [ tab ]; active_tab = 0; viewport; mode = Normal }
;;
