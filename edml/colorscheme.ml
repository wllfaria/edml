type style =
  { fg : string
  ; bg : string option
  ; italic : bool
  ; bold : bool
  ; underline : bool
  ; strikethrough : bool
  ; link : string option
  }

let color_black_0 = "#0d0c0c"
let color_black_1 = "#12120f"
let color_black_2 = "#1D1C19"
let color_black_3 = "#181616"
let color_black_4 = "#282727"
let color_black_5 = "#393836"
let color_black_6 = "#625e5a"
let color_white_0 = "#c5c9c5"
let color_white_1 = "#c5c9c5"
let color_green_0 = "#87a987"
let color_green_1 = "#8a9a7b"
let color_pink_0 = "#a292a3"
let color_orange_0 = "#b6927b"
let color_orange_1 = "#b98d7b"
let color_gray_0 = "#a6a69c"
let color_gray_2 = "#9e9b93"
let color_gray_3 = "#7a8382"
let color_blue_0 = "#223249"
let color_blue_1 = "#8ba4b0"
let color_violet = "#8992a7"
let color_red_0 = "#c4746e"
let color_aqua_0 = "#8ea4a2"
let color_ash_0 = "#737c73"
let color_teal_0 = "#949fb5"
let color_yellow_0 = "#c4b28a"

let default_style =
  { fg = color_white_0
  ; bg = Some color_black_3
  ; italic = false
  ; bold = false
  ; underline = false
  ; strikethrough = false
  ; link = None
  }
;;

let with_style
  ?fg
  ?bg
  ?(italic = default_style.italic)
  ?(bold = default_style.bold)
  ?(underline = default_style.underline)
  ?(strikethrough = default_style.strikethrough)
  ?link
  ()
  =
  { fg =
      (match fg with
       | None -> default_style.fg
       | Some v -> v)
  ; bg =
      (match bg with
       | None -> default_style.bg
       | Some v -> Some v)
  ; italic
  ; bold
  ; underline
  ; strikethrough
  ; link =
      (match link with
       | None -> default_style.link
       | Some v -> Some v)
  }
;;

let colors () =
  [ "variable", with_style ~fg:color_white_0 ()
  ; "variable.builtin", with_style ~fg:color_teal_0 ()
  ; "variable.parameter", with_style ~fg:color_gray_0 ()
  ; "variable.member", with_style ~fg:color_yellow_0 ()
  ; "string.regex", with_style ~fg:color_red_0 ()
  ; "string.escape", with_style ~fg:color_red_0 ~bold:true ()
  ; "string.special.symbol", with_style ~fg:color_yellow_0 ()
  ; "string.special.url", with_style ~fg:color_teal_0 ~underline:true ()
  ; "attribute", with_style ~link:"constant" ()
  ; "constructor", with_style ~fg:color_teal_0 ()
  ; "operator", with_style ~fg:color_red_0 ()
  ; "keyword.operator", with_style ~fg:color_red_0 ~bold:true ()
  ; "keyword.import", with_style ~fg:color_blue_0 ()
  ; "keyword.return", with_style ~fg:color_red_0 ()
  ; "keyword.exception", with_style ~fg:color_red_0 ()
  ; "keyword", with_style ~fg:color_red_0 ()
  ; "punctuation.delimiter", with_style ~fg:color_gray_2 ()
  ; "punctuation.bracket", with_style ~fg:color_gray_2 ()
  ; "punctuation.special", with_style ~fg:color_teal_0 ()
  ; "punctuation", with_style ~fg:color_gray_2 ()
  ; "comment", with_style ~fg:color_gray_3 ()
  ; "comment.error", with_style ~fg:color_white_0 ~bg:color_red_0 ()
  ; "comment.warning", with_style ~fg:color_blue_0 ~bg:color_aqua_0 ()
  ; "comment.note", with_style ~fg:color_blue_0 ~bg:color_aqua_0 ()
  ; "markup.strong", with_style ~bold:true ()
  ; "markup.italic", with_style ~italic:true ()
  ; "markup.strikethrough", with_style ~strikethrough:true ()
  ; "markup.underline", with_style ~underline:true ()
  ; "markup.heading", with_style ~link:"function" ()
  ; "markup.quote", with_style ~link:"variable.parameter" ()
  ; "markup.math", with_style ~link:"constant" ()
  ; "markup.environment", with_style ~link:"keyword" ()
  ; "markup.link.url", with_style ~link:"string.special.url" ()
  ; "markup.raw", with_style ~link:"string" ()
  ; "diff.plus", with_style ~fg:color_green_0 ()
  ; "diff.minus", with_style ~fg:color_red_0 ()
  ; "diff.delta", with_style ~fg:color_yellow_0 ()
  ; "tag.attribute", with_style ~fg:color_yellow_0 ()
  ; "tag.delimiter", with_style ~fg:color_gray_2 ()
  ; "string", with_style ~fg:"#ff0000" ()
  ; "boolean", with_style ~fg:"#ff0000" ()
  ; "character", with_style ~fg:"#ff0000" ()
  ; "conditional", with_style ~fg:"#ff0000" ()
  ; "constant.builtin", with_style ~fg:"#ff0000" ()
  ; "keyword.import", with_style ~fg:"#ff0000" ()
  ; "module", with_style ~fg:"#ff0000" ()
  ; "constant", with_style ~fg:"#ff0000" ()
  ; "constant.macro", with_style ~fg:"#ff0000" ()
  ; "debug", with_style ~fg:"#ff0000" ()
  ; "define", with_style ~fg:"#ff0000" ()
  ; "exception", with_style ~fg:"#ff0000" ()
  ; "field", with_style ~fg:"#ff0000" ()
  ; "float", with_style ~fg:"#ff0000" ()
  ; "function.builtin", with_style ~fg:"#ff0000" ()
  ; "function", with_style ~fg:"#ff0000" ()
  ; "function.macro", with_style ~fg:"#ff0000" ()
  ; "include", with_style ~fg:"#ff0000" ()
  ; "label", with_style ~fg:"#ff0000" ()
  ; "macro", with_style ~fg:"#ff0000" ()
  ; "method", with_style ~fg:"#ff0000" ()
  ; "namespace", with_style ~fg:"#ff0000" ()
  ; "number", with_style ~fg:"#ff0000" ()
  ; "parameter", with_style ~fg:"#ff0000" ()
  ; "preproc", with_style ~fg:"#ff0000" ()
  ; "property", with_style ~fg:"#ff0000" ()
  ; "repeat", with_style ~fg:"#ff0000" ()
  ; "storageclass", with_style ~fg:"#ff0000" ()
  ; "structure", with_style ~fg:"#ff0000" ()
  ; "tag", with_style ~fg:"#ff0000" ()
  ; "text", with_style ~fg:"#ff0000" ()
  ; "text.literal", with_style ~fg:"#ff0000" ()
  ; "text.reference", with_style ~fg:"#ff0000" ()
  ; "text.title", with_style ~fg:"#ff0000" ()
  ; "text.todo", with_style ~fg:"#ff0000" ()
  ; "text.underline", with_style ~fg:"#ff0000" ()
  ; "text.uri", with_style ~fg:"#ff0000" ()
  ; "type.definition", with_style ~fg:"#ff0000" ()
  ; "type", with_style ~fg:"#ff0000" ()
  ]
;;
