#!/usr/bin/env bash

if [ ! -L /etc/nixos ]; then
  sudo mv /etc/nixos /etc/nixos.orig
  sudo ln -s $HOME/Developer/scaffold/yantra/nixos/ /etc/nixos
fi

if [ ! -L $HOME/.nixpkgs ]; then
  rm -rf $HOME/.nixpkgs 2> /dev/null
  ln -s $HOME/Developer/scaffold/yantra/nixpkgs $HOME/.nixpkgs
fi

if [ ! -L $HOME/.xmonad ]; then
  rm -rf $HOME/.xmonad 2> /dev/null
  ln -s $HOME/Developer/scaffold/yantra/xmonad $HOME/.xmonad
fi

if [ ! -L $HOME/.xscreensaver ]; then
  rm -rf $HOME/.xscreensaver 2> /dev/null
  ln -s $HOME/Developer/scaffold/yantra/dotfiles/xscreensaver $HOME/.xscreensaver
fi

if [ ! -L $HOME/.config/lilyterm ]; then
  rm -rf $HOME/.config/lilyterm/ 2> /dev/null
  mkdir -p $HOME/.config/
  ln -s $HOME/Developer/scaffold/yantra/lilyterm $HOME/.config/lilyterm
fi

if [ ! -L $HOME/.config/dunst ]; then
  rm -rf $HOME/.config/dunst/ 2> /dev/null
  mkdir -p $HOME/.config/
  ln -s $HOME/Developer/scaffold/yantra/dunst $HOME/.config/dunst
fi

if [ ! -L $HOME/.fehbg ]; then
  rm -rf $HOME/.fehbg 2> /dev/null
  ln -s $HOME/Developer/scaffold/yantra/dotfiles/fehbg $HOME/.fehbg
fi
