#!/usr/bin/env bash

CURRENT_DIR=$(pwd)
EMACS_DIR=${CURRENT_DIR}/emacs

## Emacs config
mkdir -p ~/.emacs.d
ln -sf ${EMACS_DIR}/.emacs.d/init.el ~/.emacs.d/init.el
echo "Emacs config initialized"

