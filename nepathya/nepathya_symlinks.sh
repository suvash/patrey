#!/usr/bin/env bash

 mkdir -p $HOME/.config

if [ ! -L $HOME/.config/karabiner ]; then
  rm -rf $HOME/.config/karabiner 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/nepathya/karabiner $HOME/.config/karabiner
fi

if [ ! -L $HOME/.tmux-macos.conf ]; then
  rm $HOME/.tmux-macos.conf 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/nepathya/dotfiles/tmux-macos.conf $HOME/.tmux-macos.conf
fi

if [ ! -L $HOME/.hushlogin ]; then
  rm $HOME/.hushlogin 2> /dev/null
  ln -sfv $HOME/Developer/scaffold/nepathya/dotfiles/hushlogin $HOME/.hushlogin
fi

if [ ! -L $HOME/.gnupg/gpg-agent.conf ]; then
  mkdir -p $HOME/.gnupg && chmod 700 $HOME/.gnupg
  ln -sfv $HOME/Developer/scaffold/nepathya/dotfiles/gpg-agent.conf $HOME/.gnupg/gpg-agent.conf
fi
