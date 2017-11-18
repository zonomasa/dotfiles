#!/bin/sh
# expect dir structure : ~/dotfiles

set -u

ln -s ~/dotfiles/.emacs.d ~
ln -s ~/dotfiles/.zshenv ~
ln -s ~/dotfiles/.zshrc ~

mkdir -p ~/github
git clone https://github.com/zsh-users/zaw.git ~/github/zaw
