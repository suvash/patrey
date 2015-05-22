#!/usr/bin/env bash

if [ ! -L $HOME/.tmux-osx.conf ]; then
  rm $HOME/tmux-osx.conf 2> /dev/null
  ln -s $HOME/Developer/scaffold/nepathya/dotfiles/tmux-osx.conf $HOME/.tmux-osx.conf
fi
