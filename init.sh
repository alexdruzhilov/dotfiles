#!/usr/bin/env bash

CURRENT_DIR=$(pwd)

## Emacs config
EMACS_DIR=${CURRENT_DIR}/emacs
mkdir -p ~/.emacs.d
ln -sf ${EMACS_DIR}/.emacs.d/init.el ~/.emacs.d/init.el
echo "Emacs config initialized"

## Vim config
VIM_DIR=${CURRENT_DIR}/vim
ln -sf ${VIM_DIR}/.vimrc ~/.vimrc
echo "Vim config initialized"

