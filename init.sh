#!/usr/bin/env bash

CURRENT_DIR=$(pwd)

## Emacs config
mkdir -p ~/.emacs.d
ln -sf ${CURRENT_DIR}/.emacs.d/init.el ~/.emacs.d/init.el
echo "Emacs config initialized"

