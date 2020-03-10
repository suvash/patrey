#!/usr/bin/env bash

if [ ! -L "$HOME/.packages.apt" ]; then
  rm -rf "$HOME/.packages.apt" 2> /dev/null
  ln -s "$HOME/Developer/scaffold/prayogshala/packages.apt" "$HOME/.packages.apt"
fi

if [ ! -L "$HOME/.gitconfig" ]; then
  rm "$HOME/.gitconfig" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/prayogshala/dotfiles/gitconfig" "$HOME/.gitconfig"
fi

if [ ! -L "$HOME/.gitignore" ]; then
  rm "$HOME/.gitignore" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/prayogshala/dotfiles/gitignore" "$HOME/.gitignore"
fi
