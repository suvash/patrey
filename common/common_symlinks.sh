#!/usr/bin/env bash

mkdir -p $HOME/.config
mkdir -p $HOME/.config/fish

if [ ! -L $HOME/.gitconfig ]; then
  rm $HOME/.gitconfig 2> /dev/null
  ln -sfnv $HOME/Developer/scaffold/common/dotfiles/gitconfig $HOME/.gitconfig
fi

if [ ! -L $HOME/.gitignore ]; then
  rm $HOME/.gitignore 2> /dev/null
  ln -sfnv $HOME/Developer/scaffold/common/dotfiles/gitignore $HOME/.gitignore
fi

if [ ! -L $HOME/.config/fish/config.fish ]; then
  rm -rf $HOME/.config/fish/config.fish 2> /dev/null
  rm -rf $HOME/.config/fish/fishfile 2> /dev/null
  ln -sfnv $HOME/Developer/scaffold/common/fish/config.fish $HOME/.config/fish/config.fish
  ln -sfnv $HOME/Developer/scaffold/common/fish/fishfile $HOME/.config/fish/fishfile
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

if [ ! -L $HOME/.tmux.conf ]; then
  rm $HOME/tmux.conf 2> /dev/null
  ln -sfnv $HOME/Developer/scaffold/common/dotfiles/tmux.conf $HOME/.tmux.conf
fi

if [ ! -L $HOME/.emacs.d ]; then
  rm -rf $HOME/.emacs.d/ 2> /dev/null
  ln -sfnv $HOME/Developer/scaffold/common/emacs.d $HOME/.emacs.d
fi

if [ ! -L $HOME/.vim ]; then
  rm -rf $HOME/.vim 2> /dev/null
  ln -sfnv $HOME/Developer/scaffold/common/vim $HOME/.vim
fi

if [ ! -L $HOME/.wallpapers ]; then
  rm -rf $HOME/.wallpapers 2> /dev/null
  ln -sfnv $HOME/Developer/scaffold/common/wallpapers $HOME/.wallpapers
fi

if [ ! -L $HOME/.gnupg/gpg.conf ]; then
  mkdir -p $HOME/.gnupg && touch $HOME/.gnupg/gpg.conf && mv $HOME/.gnupg/gpg.conf $HOME/.gnupg/gpg.conf.old && chmod 700 $HOME/.gnupg
  ln -sfnv $HOME/Developer/scaffold/common/dotfiles/gpg.conf $HOME/.gnupg/gpg.conf
fi
