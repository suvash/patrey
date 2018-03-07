#!/usr/bin/env bash

mkdir -p $HOME/.local/bin/
mkdir -p $HOME/.config/
mkdir -p $HOME/.config/autorandr

if [ ! -L /etc/nixos ]; then
  sudo mv /etc/nixos /etc/nixos.orig
  sudo ln -sfv $HOME/Developer/scaffold/yantra/nixos/ /etc/nixos
fi

if [ ! -L $HOME/.config/nixpkgs ]; then
  rm -rf $HOME/.config/nixpkgs 2> /dev/null
  sudo ln -sfv $HOME/Developer/scaffold/yantra/nixpkgs/ $HOME/.config/nixpkgs
fi

if [ ! -L $HOME/.xinitrc ]; then
  rm -rf $HOME/.xinitrc 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/yantra/dotfiles/xinitrc $HOME/.xinitrc
fi

if [ ! -L $HOME/.Xresources ]; then
  rm -rf $HOME/.Xresources 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/yantra/dotfiles/Xresources $HOME/.Xresources
fi

if [ ! -L $HOME/.local/bin/autoconfigure-workstation ]; then
  rm -rf $HOME/.local/bin/autoconfigure-workstation 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/yantra/scripts/autoconfigure-workstation $HOME/.local/bin/autoconfigure-workstation
fi

if [ ! -L $HOME/.local/bin/utf-demo ]; then
  rm -rf $HOME/.local/bin/utf-demo 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/yantra/scripts/utf-demo $HOME/.local/bin/utf-demo
fi

if [ ! -L $HOME/.xmonad ]; then
  rm -rf $HOME/.xmonad 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/yantra/xmonad $HOME/.xmonad
fi

if [ ! -L $HOME/.config/autorandr/preswitch ]; then
  rm -rf $HOME/.config/autorandr/preswitch 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/yantra/autorandr/preswitch $HOME/.config/autorandr/preswitch
fi

if [ ! -L $HOME/.config/autorandr/postswitch ]; then
  rm -rf $HOME/.config/autorandr/postswitch 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/yantra/autorandr/postswitch $HOME/.config/autorandr/postswitch
fi

if [ ! -L $HOME/.xmobarrc ]; then
  rm -rf $HOME/.xmobarrc 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/yantra/dotfiles/xmobarrc $HOME/.xmobarrc
fi

if [ ! -L $HOME/.stalonetrayrc ]; then
  rm -rf $HOME/.stalonetrayrc 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/yantra/dotfiles/stalonetrayrc $HOME/.stalonetrayrc
fi

if [ ! -L $HOME/.xscreensaver ]; then
  rm -rf $HOME/.xscreensaver 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/yantra/dotfiles/xscreensaver $HOME/.xscreensaver
fi

if [ ! -L $HOME/.config/dunst ]; then
  rm -rf $HOME/.config/dunst/ 2> /dev/null
  mkdir -p $HOME/.config/
  ln -sfv $HOME/Developer/scaffold/yantra/dunst $HOME/.config/dunst
fi

if [ ! -L $HOME/.config/xfce4 ]; then
  rm -rf $HOME/.config/xfce4 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/yantra/xfce4 $HOME/.config/xfce4
fi

if [ ! -L $HOME/.config/lilyterm ]; then
  rm -rf $HOME/.config/lilyterm 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/yantra/lilyterm $HOME/.config/lilyterm
fi

if [ ! -L $HOME/.fehbg ]; then
  rm -rf $HOME/.fehbg 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/yantra/dotfiles/fehbg $HOME/.fehbg
fi
