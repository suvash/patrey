#!/usr/bin/env bash

if [ ! -L $HOME/.nixpkgs ]; then
  rm -rf $HOME/.nixpkgs 2> /dev/null
  ln -s $HOME/Developer/scaffold/yantra/nixpkgs $HOME/.nixpkgs
fi

if [ ! -L $HOME/.autorandr ]; then
  rm -rf $HOME/.autorandr 2> /dev/null
  ln -s $HOME/Developer/scaffold/yantra/autorandr $HOME/.autorandr
fi

if [ ! -L $HOME/.packages.apt ]; then
  rm -rf $HOME/.packages.apt 2> /dev/null
  ln -s $HOME/Developer/scaffold/yantra/packages.apt $HOME/.packages.apt
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

if [ ! -L /usr/local/bin/start-xmonad-session ]; then
  rm -rf /usr/local/bin/start-xmonad-session 2> /dev/null
  sudo ln -s $HOME/Developer/scaffold/yantra/scripts/start-xmonad-session /usr/local/bin/start-xmonad-session
fi

if [ ! -L /usr/local/bin/start-tor-browser ]; then
  rm -rf /usr/local/bin/start-tor-browser 2> /dev/null
  sudo ln -s $HOME/Developer/scaffold/yantra/scripts/start-tor-browser /usr/local/bin/start-tor-browser
fi

if [ ! -L /usr/local/bin/start-rstudio ]; then
  rm -rf /usr/local/bin/start-rstudio 2> /dev/null
  sudo ln -s $HOME/Developer/scaffold/yantra/scripts/start-rstudio /usr/local/bin/start-rstudio
fi

if [ ! -L /usr/local/bin/start-viber ]; then
  rm -rf /usr/local/bin/start-viber 2> /dev/null
  sudo ln -s $HOME/Developer/scaffold/yantra/scripts/start-viber /usr/local/bin/start-viber
fi

if [ ! -L /usr/local/bin/set-greeter-resolution ]; then
  rm -rf /usr/local/bin/set-greeter-resolution 2> /dev/null
  sudo ln -s $HOME/Developer/scaffold/yantra/scripts/set-greeter-resolution /usr/local/bin/set-greeter-resolution
fi

if [ ! -L /usr/local/bin/autoconfigure-workstation ]; then
  rm -rf /usr/local/bin/autoconfigure-workstation 2> /dev/null
  sudo ln -s $HOME/Developer/scaffold/yantra/scripts/autoconfigure-workstation /usr/local/bin/autoconfigure-workstation
fi
