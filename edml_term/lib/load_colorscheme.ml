open Core

type style =
  { fg : Ansi.Color.t
  ; bg : Ansi.Color.t
  }
[@@deriving eq, show { with_path = false }]

let empty_style () = { fg = Ansi.Color.Reset; bg = Ansi.Color.Reset }
let colors = ref @@ Hashtbl.create (module String)

let load_colors () =
  let map = Hashtbl.create (module String) in
  let to_link = ref [] in
  List.iter (Edml.Colorscheme.colors ()) ~f:(fun (name, style) ->
    match style.link with
    | Some style -> to_link := (name, style) :: !to_link
    | None ->
      let fg = style.fg in
      let bg = Option.value style.bg ~default:"resetbg" in
      ignore
      @@ Hashtbl.add
           map
           ~key:name
           ~data:{ fg = Ansi.Color.from fg; bg = Ansi.Color.from bg });
  List.iter !to_link ~f:(fun (name, link_name) ->
    let style = Hashtbl.find_exn map link_name in
    ignore @@ Hashtbl.add map ~key:name ~data:style);
  colors := map
;;
