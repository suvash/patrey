#!/usr/bin/env bash

if [ ! -L $HOME/.tmux.conf ]; then
  rm $HOME/tmux.conf 2> /dev/null
  ln -s $HOME/Developer/scaffold/bajra/dotfiles/tmux.conf $HOME/.tmux.conf
fi
