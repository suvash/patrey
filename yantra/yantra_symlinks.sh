#!/usr/bin/env bash

if [ ! -L $HOME/.nixpkgs ]; then
  rm -rf $HOME/.nixpkgs 2> /dev/null
  ln -s $HOME/Developer/scaffold/yantra/nixpkgs $HOME/.nixpkgs
fi

if [ ! -L $HOME/.autorandr ]; then
  rm -rf $HOME/.autorandr 2> /dev/null
  ln -s $HOME/Developer/scaffold/yantra/autorandr $HOME/.autorandr
fi

if [ ! -L /etc/lightdm/lightdm.conf ]; then
  rm -rf /etc/lightdm/lightdm.conf 2> /dev/null
  sudo ln -s $HOME/Developer/scaffold/yantra/lightdm/lightdm.conf /etc/lightdm/lightdm.conf
fi

if [ ! -L /usr/share/xsessions/xmonad.desktop ]; then
  mv /usr/share/xsessions/xmonad.desktop /usr/share/xsessions/xmonad.desktop.orig 2> /dev/null
  sudo ln -s $HOME/Developer/scaffold/yantra/assets/xmonad.desktop /usr/share/xsessions/xmonad.desktop
fi

if [ ! -L /usr/share/unity-greeter/xmonad_badge.png ]; then
  rm -rf /usr/share/unity-greeter/xmonad_badge.png 2> /dev/null
  sudo ln -s $HOME/Developer/scaffold/yantra/assets/xmonad_badge.png /usr/share/unity-greeter/xmonad_badge.png
fi
