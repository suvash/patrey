#!/usr/bin/env bash

mkdir -p "$HOME/.local/bin/"
mkdir -p "$HOME/.config"
mkdir -p "$HOME/.config/fish"

if [ ! -L "$HOME/.packages.apt" ]; then
  rm -rf "$HOME/.packages.apt" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/prayogshala/packages.apt" "$HOME/.packages.apt"
fi

if [ ! -L "$HOME/.gitconfig" ]; then
  rm "$HOME/.gitconfig" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/prayogshala/dotfiles/gitconfig" "$HOME/.gitconfig"
fi

if [ ! -L "$HOME/.gitignore" ]; then
  rm "$HOME/.gitignore" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/prayogshala/dotfiles/gitignore" "$HOME/.gitignore"
fi

if [ ! -L "$HOME/.config/fish/config.fish" ]; then
  rm -rf "$HOME/.config/fish/config.fish" 2> /dev/null
  rm -rf "$HOME/.config/fish/fishfile" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/common/fish/config.fish" "$HOME/.config/fish/config.fish"
  ln -sfnv "$HOME/Developer/scaffold/common/fish/fishfile" "$HOME/.config/fish/fishfile"
  mkdir -p $HOME/.config/fish/{completions,functions}
  for file in $HOME/Developer/scaffold/common/fish/completions/*
  do
      ln -sfnv $file "$HOME/.config/fish/completions/${file##*/}"
  done
  for file in $HOME/Developer/scaffold/common/fish/functions/*
  do
      ln -sfnv $file "$HOME/.config/fish/functions/${file##*/}"
  done
fi

if [ ! -L "$HOME/.emacs.d" ]; then
  rm -rf "$HOME/.emacs.d/" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/common/emacs.d" "$HOME/.emacs.d"
fi

if [ ! -L "$HOME/.vim" ]; then
  rm -rf "$HOME/.vim" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/common/vim" "$HOME/.vim"
fi
