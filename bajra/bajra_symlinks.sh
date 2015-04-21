#!/usr/bin/env bash

if [ ! -L $HOME/.tmux-osx.conf ]; then
  rm $HOME/tmux-osx.conf 2> /dev/null
  ln -s $HOME/Developer/scaffold/bajra/dotfiles/tmux-osx.conf $HOME/.tmux-osx.conf
fi

if [ ! -L $HOME/.nixpkgs ]; then
  rm -rf $HOME/.nixpkgs 2> /dev/null
  ln -s $HOME/Developer/scaffold/bajra/nixpkgs $HOME/.nixpkgs
fi
