#!/usr/bin/env bash

CURRENT_DIR=$(pwd)

## Vim config
function vim_config {
    VIM_DIR=${CURRENT_DIR}/vim
    ln -sf ${VIM_DIR}/.vimrc ~/.vimrc
    echo "Vim config initialized"
}


## Emacs config
function emacs_config {
    EMACS_DIR=${CURRENT_DIR}/emacs
    mkdir -p ~/.emacs.d
    ln -sf ${EMACS_DIR}/.emacs.d/init.el ~/.emacs.d/init.el
    echo "Emacs config initialized"
}

## Bash config
function bash_config {
    BASH_DIR=${CURRENT_DIR}/bash
    ln -sf ${BASH_DIR}/.bash_aliases ~/.bash_aliases
    source ~/.bashrc
    echo "Bash config initialized"
}

## Git config
function git_config {
    GIT_DIR=${CURRENT_DIR}/git
    ln -sf ${GIT_DIR}/.gitconfig ~/.gitconfig
    echo "Git config initialized"
}

## Tmux config
function tmux_config {
    GIT_DIR=${CURRENT_DIR}/tmux
    ln -sf ${GIT_DIR}/.tmux.conf ~/.tmux.conf
    echo "Tmux config initialized"
}

case "$1" in
    vim) vim_config
        ;;
    emacs) emacs_config
        ;;
    bash) bash_config
        ;;
    git) git_config
        ;;
    tmux) tmux_config
        ;;
    *) echo "Please specify config name (vim|emacs|tmux|bash|git)"
        ;;
esac
