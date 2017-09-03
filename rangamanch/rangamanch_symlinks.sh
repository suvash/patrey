#!/usr/bin/env bash

if [ ! -L $HOME/.xinitrc ]; then
  rm -rf $HOME/.xinitrc 2> /dev/null
  ln -s $HOME/Developer/scaffold/rangamanch/dotfiles/xinitrc $HOME/.xinitrc
fi

if [ ! -L $HOME/.Xresources ]; then
  rm -rf $HOME/.Xresources 2> /dev/null
  ln -s $HOME/Developer/scaffold/rangamanch/dotfiles/Xresources $HOME/.Xresources
fi

if [ ! -L $HOME/.env ]; then
  rm -rf $HOME/.env 2> /dev/null
  ln -s $HOME/Developer/scaffold/rangamanch/dotfiles/env $HOME/.env
fi

if [ ! -L $HOME/.packages.apt ]; then
  rm -rf $HOME/.packages.apt 2> /dev/null
  ln -s $HOME/Developer/scaffold/rangamanch/packages.apt $HOME/.packages.apt
fi

if [ ! -L /usr/share/xsessions/xmonad.desktop ]; then
  sudo mv /usr/share/xsessions/xmonad.desktop /usr/share/xsessions/xmonad.desktop.orig 2> /dev/null
  sudo ln -s $HOME/Developer/scaffold/rangamanch/assets/xmonad.desktop /usr/share/xsessions/xmonad.desktop
fi

if [ ! -L /usr/local/bin/start-xmonad-session ]; then
  sudo rm -rf /usr/local/bin/start-xmonad-session 2> /dev/null
  sudo ln -s $HOME/Developer/scaffold/rangamanch/scripts/start-xmonad-session /usr/local/bin/start-xmonad-session
fi

if [ ! -L $HOME/.local/bin/autoconfigure-workstation ]; then
  rm -rf $HOME/.local/bin/autoconfigure-workstation 2> /dev/null
  ln -s $HOME/Developer/scaffold/rangamanch/scripts/autoconfigure-workstation $HOME/.local/bin/autoconfigure-workstation
fi

if [ ! -L $HOME/.local/bin/get-module-parameters ]; then
  rm -rf $HOME/.local/bin/get-module-parameters 2> /dev/null
  ln -s $HOME/Developer/scaffold/rangamanch/scripts/get-module-parameters $HOME/.local/bin/get-module-parameters
fi

if [ ! -L $HOME/.local/bin/utf-demo ]; then
  rm -rf $HOME/.local/bin/utf-demo 2> /dev/null
  ln -s $HOME/Developer/scaffold/rangamanch/scripts/utf-demo $HOME/.local/bin/utf-demo
fi

if [ ! -L $HOME/.gnupg/gpg-agent.conf ]; then
  mkdir -p $HOME/.gnupg && chmod 700 $HOME/.gnupg
  ln -s $HOME/Developer/scaffold/rangamanch/dotfiles/gpg-agent.conf $HOME/.gnupg/gpg-agent.conf
fi
