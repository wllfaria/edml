#!/bin/sh

GRAMMARS_DIR="$(pwd)/vendor/tree_sitter"
GRAMMARS=$(ls $GRAMMARS_DIR)
LANGUAGES_DIR="$(pwd)/languages"

make_languages_dir() {
    if [[ ! -d $LANGUAGES_DIR ]]; then
        mkdir $LANGUAGES_DIR
    fi
}

make_gramar_language_dir() {
    grammar=$1
    if [[ ! $grammar ]]; then
        echo "no grammar provided exiting"
        exit 1
    fi

    language_path="$LANGUAGES_DIR/$grammar"

    if [[ ! -d $language_path ]]; then
        mkdir $language_path
    fi
}

build_grammars() {
    for grammar in $GRAMMARS; do
        cd "$GRAMMARS_DIR/$grammar"
        header_path="$GRAMMARS_DIR/$grammar/bindings/c"
        header_name="tree-sitter-$grammar.h"
        header_file="$header_path/$header_name"
        object_path="$GRAMMARS_DIR/$grammar/libtree-sitter-$grammar.a"

        make
        make_gramar_language_dir $grammar

        language_path="$LANGUAGES_DIR/$grammar"
        cp -f $header_file $language_path
        cp -f $object_path $language_path
    done
}

make_languages_dir
build_grammars
