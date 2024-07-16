type editor =
  { tabs : tab list
  ; viewport : Viewport.t ref
  ; mode : mode
  ; buffers : Text_buffer.t list
  ; active_tab : int
  }

and mode =
  | Normal
  | Insert

and tab =
  { panes : pane_tree
  ; active_pane : int
  }

and pane_tree =
  | Single of Pane.t
  | Split of pane_branch

and pane_branch =
  { direction : direction
  ; ratios : float list
  ; panes : pane_tree list
  }

and direction =
  | Horizontal
  | Vertical

type t = editor

let make ~pane ~active_pane ~buffer ~viewport =
  let tab = { panes = pane; active_pane } in
  { buffers = [ buffer ]; tabs = [ tab ]; active_tab = 0; viewport; mode = Normal }
;;
