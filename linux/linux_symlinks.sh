#!/usr/bin/env bash

if [ ! -L $HOME/.xmonad ]; then
  rm -rf $HOME/.xmonad 2> /dev/null
  ln -s $HOME/Developer/scaffold/linux/xmonad $HOME/.xmonad
fi

if [ ! -L $HOME/.xscreensaver ]; then
  rm -rf $HOME/.xscreensaver 2> /dev/null
  ln -s $HOME/Developer/scaffold/linux/dotfiles/xscreensaver $HOME/.xscreensaver
fi

if [ ! -L $HOME/.config/dunst ]; then
  rm -rf $HOME/.config/dunst/ 2> /dev/null
  mkdir -p $HOME/.config/
  ln -s $HOME/Developer/scaffold/linux/dunst $HOME/.config/dunst
fi

if [ ! -L $HOME/.fehbg ]; then
  rm -rf $HOME/.fehbg 2> /dev/null
  ln -s $HOME/Developer/scaffold/linux/dotfiles/fehbg $HOME/.fehbg
fi
