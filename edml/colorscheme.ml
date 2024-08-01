type style =
  { fg : string
  ; bg : string option
  ; italic : bool
  ; bold : bool
  ; underline : bool
  ; strikethrough : bool
  ; link : string option
  }

let default_style =
  { fg = "#ece1d7"
  ; bg = Some "#292522"
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
  [ "attribute", with_style ~fg:"#8b7449" ()
  ; "operator", with_style ~fg:"#d47766" ()
  ; "tag", with_style ~fg:"#8b7449" ()
  ; "tag.attribute", with_style ~fg:"#ebc06d" ()
  ; "tag.delimiter", with_style ~fg:"#8d8183" ()
  ; "boolean", with_style ~link:"number" ()
  ; "number", with_style ~fg:"#cf9bc2" ()
  ; "identifier", with_style ~fg:"#ECE1D7" ()
  ; "number.float", with_style ~link:"number" ()
  ; "character", with_style ~fg:"#7f91b2" ()
  ; "character.special", with_style ~fg:"#ebc06d" ()
  ; "string", with_style ~fg:"#a3a9ce" ()
  ; "string.documentation", with_style ~fg:"#a3a9ce" ()
  ; "string.escape", with_style ~fg:"#7f91b2" ()
  ; "constructor", with_style ~fg:"#EBC06D" ()
  ; "string.regexp", with_style ~fg:"#a3a9ce" ()
  ; "string.special", with_style ~fg:"#89b3b6" ()
  ; "string.special.symbol", with_style ~fg:"#ece1d7" ~italic:true ()
  ; "string.special.path", with_style ~fg:"#7f91b2" ()
  ; "string.special.url", with_style ~fg:"#7f91b2" ()
  ; "keyword", with_style ~fg:"#E49B5D" ()
  ; "keyword.type", with_style ~fg:"#E49B5D" ()
  ; "keyword.operator", with_style ~fg:"#8d8183" ~bold:true ()
  ; "keyword.import", with_style ~fg:"#b380b0" ()
  ; "keyword.return", with_style ~fg:"#b380b0" ()
  ; "keyword.exception", with_style ~fg:"#8d8183" ()
  ; "constant", with_style ~fg:"#B380B0" ()
  ; "constant.builtin", with_style ~fg:"#ff0000" ()
  ; "constant.macro", with_style ~fg:"#ece1d7" ()
  ; "module", with_style ~fg:"#ece1d7" ()
  ; "module.builtin", with_style ~link:"module" ()
  ; "label", with_style ~fg:"#89b3b6" ()
  ; "variable", with_style ~fg:"#ece1d7" ()
  ; "variable.builtin", with_style ~link:"string.special.symbol" ()
  ; "variable.parameter", with_style ~fg:"#ece1d7" ()
  ; "variable.member", with_style ~fg:"#ece1d7" ()
  ; "type", with_style ~fg:"#7b9695" ()
  ; "type.builtin", with_style ~link:"type" ()
  ; "type.definition", with_style ~fg:"#7b9695" ()
  ; "type.qualifier", with_style ~fg:"#8b7449" ()
  ; "function", with_style ~fg:"#ebc06d" ()
  ; "function.builtin", with_style ~fg:"#ebc06d" ()
  ; "function.macro", with_style ~link:"function" ()
  ; "statement", with_style ~fg:"#E49B5D" ()
  ; "punctuation", with_style ~fg:"#8b7449" ()
  ; "punctuation.delimiter", with_style ~fg:"#BD8183" ()
  ; "punctuation.bracket", with_style ~fg:"#8b7449" ()
  ; "punctuation.special", with_style ~fg:"#ebc06d" ()
  ; "comment", with_style ~fg:"#c1a783" ()
  ; "comment.todo", with_style ~fg:"#c1a783" ~bold:true ()
  ; "comment.error", with_style ~link:"comment.todo" ()
  ; "comment.warning", with_style ~link:"comment.todo" ()
  ; "comment.note", with_style ~link:"comment.todo" ()
  ; "markup.heading", with_style ~fg:"#8b7449" ()
  ; "markup.heading.2", with_style ~fg:"#ebc06d" ()
  ; "markup.heading.3", with_style ~fg:"#85b695" ()
  ; "markup.heading.4", with_style ~fg:"#89b3b6" ()
  ; "markup.strong", with_style ~bold:true ()
  ; "markup.italic", with_style ~italic:true ()
  ; "markup.strikethrough", with_style ~strikethrough:true ()
  ; "markup.underline", with_style ~underline:true ()
  ; "markup.quote", with_style ~link:"comment" ()
  ; "markup.math", with_style ~fg:"#8b7449" ()
  ; "markup.link", with_style ~link:"string.special.url" ()
  ; "markup.link.url", with_style ~link:"string.special.url" ()
  ; "markup.raw", with_style ~fg:"#c1a783" ()
  ; "conditional", with_style ~fg:"#8b7449" ()
  ; "debug", with_style ~fg:"#ebc06d" ()
  ; "define", with_style ~fg:"#ebc06d" ()
  ; "exception", with_style ~fg:"#8b7449" ()
  ; "float", with_style ~link:"number" ()
  ; "include", with_style ~fg:"#8b7449" ()
  ; "macro", with_style ~fg:"#8b7449" ()
  ; "preproc", with_style ~fg:"#8b7449" ()
  ; "property", with_style ~fg:"#ece1d7" ()
  ; "repeat", with_style ~fg:"#8b7449" ()
  ; "storageclass", with_style ~fg:"#7b9695" ()
  ; "structure", with_style ~fg:"#7b9695" ()
  ; "tag", with_style ~fg:"#8b7449" ()
  ]
;;
