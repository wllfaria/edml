(lang dune 3.16)

(name edml)

(generate_opam_files true)

(source
 (github wllfaria/edml))

(authors "wiru")

(maintainers "wiru")

(license MIT)

(documentation https://github.com/wllfaria/edml)

(package
 (name edml)
 (synopsis "silly terminal text editor")
 (description "in ocaml!")
 (depends ocaml dune)
 (tags
  (topics terminal editor "text editor")))

(package
 (name ansi)
 (synopsis "silly terminal text editor")
 (description "in ocaml!")
 (depends ocaml dune)
 (tags
  (topics terminal editor "text editor")))

(package
 (name ewww)
 (synopsis "silly terminal text editor")
 (description "in ocaml!")
 (depends ocaml dune)
 (tags
  (topics terminal editor "text editor")))

