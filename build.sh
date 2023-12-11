#!/bin/bash

set -e

readonly EMACS_DIR="$(dirname "$(readlink -f "$0")")"
readonly LOAD_PATH=$EMACS_DIR/etc
readonly BUILD_PATH=$EMACS_DIR/build

PACK_NAME=emacs-x
PACK_SYSTEM=x86_64-linux
export EMACS_CONFIG_MODE=minimal

if [[ ! -z $1 ]] ; then
    PACK_NAME=$1
fi

if [[ ! -z $2 ]] ; then
    PACK_SYSTEM=$1
fi

PACK_FILENAME="${PACK_NAME}-${EMACS_CONFIG_MODE}-${PACK_SYSTEM}.tar.gz"
INSTALLER_FILENAME="${PACK_NAME}-${EMACS_CONFIG_MODE}-${PACK_SYSTEM}-installer.run"

echo "Emacs dir: $EMACS_DIR"
echo "Pack name: $PACK_FILENAME"

echo "Start build emacs-git"

guix build -K -L $LOAD_PATH emacs-git

echo "Start install emacs-git"

guix install -L $LOAD_PATH emacs-git

echo "Start install tree sitter grammers"

if [[ ! -d $BUILD_PATH ]] ; then
    mkdir $BUILD_PATH
fi

pushd $BUILD_PATH

guix pack -RR -r tree-sitter-grammar.tar.gz -S /opt/tree-sitter/lib=lib \
     tree-sitter-bash \
     tree-sitter-bibtex \
     tree-sitter-c \
     tree-sitter-c-sharp \
     tree-sitter-clojure \
     tree-sitter-cmake \
     tree-sitter-cpp \
     tree-sitter-css \
     tree-sitter-dockerfile \
     tree-sitter-elisp \
     tree-sitter-elixir \
     tree-sitter-elm \
     tree-sitter-go \
     tree-sitter-gomod \
     tree-sitter-haskell \
     tree-sitter-heex \
     tree-sitter-html \
     tree-sitter-java \
     tree-sitter-javascript \
     tree-sitter-json \
     tree-sitter-julia \
     tree-sitter-lua \
     tree-sitter-markdown \
     tree-sitter-markdown-gfm \
     tree-sitter-meson \
     tree-sitter-ocaml \
     tree-sitter-org \
     tree-sitter-php \
     tree-sitter-plantuml \
     tree-sitter-python \
     tree-sitter-r \
     tree-sitter-racket \
     tree-sitter-ruby \
     tree-sitter-rust \
     tree-sitter-scala \
     tree-sitter-scheme \
     tree-sitter-typescript

tar xf tree-sitter-grammar.tar.gz

if [[ ! -d $EMACS_DIR/tree-sitter ]] ; then
    mkdir $EMACS_DIR/tree-sitter
fi

cp -f opt/tree-sitter/lib/tree-sitter/* $EMACS_DIR/tree-sitter/
popd

echo "Start init emacs-config"

GUIX_PROFILE="${HOME}/.guix-profile"
source "${GUIX_PROFILE}/etc/profile"

emacs --init-directory=$EMACS_DIR -Q --batch -l org --eval "(org-babel-tangle-file \"README.org\")"

emacs --init-directory=$EMACS_DIR -Q --batch -l early-init.el -l init.el

echo "Start build emacs-config"
guix build -K -L $LOAD_PATH emacs-config

echo "Start install emacs-config"
guix install -L $LOAD_PATH emacs-config

echo "Start build glibc-zh-utf8-locales"
guix build -K -L $LOAD_PATH glibc-zh-utf8-locales

echo "Start install glibc-zh-utf8-locales"
guix install -L $LOAD_PATH glibc-zh-utf8-locales

echo "Start pack"

guix pack -L $LOAD_PATH -RR -r $PACK_FILENAME -S /opt/emacs/bin=bin -S /opt/emacs/lib=lib -S /opt/emacs/etc=etc -S /opt/emacs/share=share \
     glibc-zh-utf8-locales emacs-git emacs-config
     
echo "Generate installer"

cat install.sh $PACK_FILENAME > $INSTALLER_FILENAME
