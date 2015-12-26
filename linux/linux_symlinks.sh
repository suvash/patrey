#!/usr/bin/env bash

if [ ! -L $HOME/.xmonad ]; then
  rm -rf $HOME/.xmonad 2> /dev/null
  ln -s $HOME/Developer/scaffold/linux/xmonad $HOME/.xmonad
fi

if [ ! -L $HOME/.xmobarrc ]; then
  rm -rf $HOME/.xmobarrc 2> /dev/null
  ln -s $HOME/Developer/scaffold/linux/dotfiles/xmobarrc $HOME/.xmobarrc
fi

if [ ! -L $HOME/.stalonetrayrc ]; then
  rm -rf $HOME/.stalonetrayrc 2> /dev/null
  ln -s $HOME/Developer/scaffold/linux/dotfiles/stalonetrayrc $HOME/.stalonetrayrc
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

if [ ! -L $HOME/.config/xfce4 ]; then
  rm -rf $HOME/.config/xfce4 2> /dev/null
  ln -s $HOME/Developer/scaffold/linux/xfce4 $HOME/.config/xfce4
fi

if [ ! -L $HOME/.config/lilyterm ]; then
  rm -rf $HOME/.config/lilyterm 2> /dev/null
  ln -s $HOME/Developer/scaffold/linux/lilyterm $HOME/.config/lilyterm
fi

if [ ! -L $HOME/.fehbg ]; then
  rm -rf $HOME/.fehbg 2> /dev/null
  ln -s $HOME/Developer/scaffold/linux/dotfiles/fehbg $HOME/.fehbg
fi
