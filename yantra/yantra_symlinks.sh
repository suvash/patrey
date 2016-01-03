#!/usr/bin/env bash

set -euf -o pipefail

if [ ! -L $HOME/.nixpkgs ]; then
  rm -rf $HOME/.nixpkgs
  ln -s $HOME/Developer/scaffold/yantra/nixpkgs $HOME/.nixpkgs
fi

if [ ! -L $HOME/.autorandr ]; then
  rm -rf $HOME/.autorandr
  ln -s $HOME/Developer/scaffold/yantra/autorandr $HOME/.autorandr
fi

if [ ! -L $HOME/.packages.apt ]; then
  rm -rf $HOME/.packages.apt
  ln -s $HOME/Developer/scaffold/yantra/packages.apt $HOME/.packages.apt
fi

if [ ! -L /etc/rc.local ]; then
  sudo mv /etc/rc.local /etc/rc.local.orig
  sudo ln -s $HOME/Developer/scaffold/yantra/rc.local /etc/rc.local
fi

if [ ! -L /etc/lightdm/lightdm.conf ]; then
  rm -rf /etc/lightdm/lightdm.conf
  sudo ln -s $HOME/Developer/scaffold/yantra/lightdm/lightdm.conf /etc/lightdm/lightdm.conf
fi

if [ ! -L /usr/share/xsessions/xmonad.desktop ]; then
  mv /usr/share/xsessions/xmonad.desktop /usr/share/xsessions/xmonad.desktop.orig
  sudo ln -s $HOME/Developer/scaffold/yantra/assets/xmonad.desktop /usr/share/xsessions/xmonad.desktop
fi

if [ ! -L /usr/share/unity-greeter/xmonad_badge.png ]; then
  rm -rf /usr/share/unity-greeter/xmonad_badge.png
  sudo ln -s $HOME/Developer/scaffold/yantra/assets/xmonad_badge.png /usr/share/unity-greeter/xmonad_badge.png
fi

if [ ! -L /usr/local/bin/start-xmonad-session ]; then
  rm -rf /usr/local/bin/start-xmonad-session
  sudo ln -s $HOME/Developer/scaffold/yantra/scripts/start-xmonad-session /usr/local/bin/start-xmonad-session
fi

if [ ! -L /usr/local/bin/start-tor-browser ]; then
  rm -rf /usr/local/bin/start-tor-browser
  sudo ln -s $HOME/Developer/scaffold/yantra/scripts/start-tor-browser /usr/local/bin/start-tor-browser
fi

if [ ! -L /usr/local/bin/start-rstudio ]; then
  rm -rf /usr/local/bin/start-rstudio
  sudo ln -s $HOME/Developer/scaffold/yantra/scripts/start-rstudio /usr/local/bin/start-rstudio
fi

if [ ! -L /usr/local/bin/start-viber ]; then
  rm -rf /usr/local/bin/start-viber
  sudo ln -s $HOME/Developer/scaffold/yantra/scripts/start-viber /usr/local/bin/start-viber
fi

if [ ! -L /usr/local/bin/set-greeter-resolution ]; then
  rm -rf /usr/local/bin/set-greeter-resolution
  sudo ln -s $HOME/Developer/scaffold/yantra/scripts/set-greeter-resolution /usr/local/bin/set-greeter-resolution
fi
