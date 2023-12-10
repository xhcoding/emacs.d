#!/bin/bash

set -e

readonly EMACS_DIR="$(dirname "$(readlink -f "$0")")"
readonly LOAD_PATH=$EMACS_DIR/etc

PACK_NAME=emacs-x
PACK_SYSTEM=x86_64-linux

if [[ ! -z $1 ]] ; then
    PACK_NAME=$1
fi

if [[ ! -z $2 ]] ; then
    PACK_SYSTEM=$1
fi

PACK_FILENAME="${PACK_NAME}-${PACK_SYSTEM}.tar.gz"
INSTALLER_FILENAME="${PACK_NAME}-${PACK_SYSTEM}-installer.run"

echo "Emacs dir: $EMACS_DIR"
echo "Pack name: $PACK_FILENAME"

echo "Start build emacs-git"

guix build -K -L $LOAD_PATH emacs-git

echo "Start install emacs-git"

guix install -L $LOAD_PATH emacs-git

echo "Start init emacs-config"

GUIX_PROFILE="${HOME}/.guix-profile"
source "${GUIX_PROFILE}/etc/profile"

emacs --init-directory=$EMACS_DIR -Q --batch -l org --eval "(org-babel-tangle-file \"README.org\")"

emacs --init-directory=$EMACS_DIR -Q --batch -l early-init.el -l init.el

echo "Start build emacs-config"
guix build -K -L $LOAD_PATH emacs-config

echo "Start install emacs-config"
guix install -L $LOAD_PATH emacs-config

echo "Start pack"

guix pack -L $LOAD_PATH -RR -r $PACK_FILENAME -S /opt/emacs/bin=bin -S /opt/emacs/lib=lib -S /opt/emacs/etc=etc -S /opt/emacs/share=share \
     glibc-locales emacs-git emacs-config
     
echo "Generate installer"

cat install.sh $PACK_FILENAME > $INSTALLER_FILENAME
