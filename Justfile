set positional-arguments
set export

TREE_SITTER_LIB := "/opt/homebrew/Cellar/tree-sitter/0.22.6/lib"
TREE_SITTER_INC := "/opt/homebrew/Cellar/tree-sitter/0.22.6/include"

@build:
    dune build @check -w

@exec:
    dune exec edml

@execw:
    dune exec edml -w
