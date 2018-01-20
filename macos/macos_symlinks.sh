#!/usr/bin/env bash

if [ ! -L $HOME/.tmux-macos.conf ]; then
  rm $HOME/.tmux-macos.conf 2> /dev/null
  ln -s $HOME/Developer/scaffold/macos/tmux-macos.conf $HOME/.tmux-macos.conf
fi

if [ ! -L $HOME/.mackup.cfg ]; then
  rm $HOME/.mackup.cfg 2> /dev/null
  ln -s $HOME/Developer/scaffold/macos/mackup.cfg $HOME/.mackup.cfg
fi

if [ ! -L $HOME/.karabiner.d ]; then
  rm -rf $HOME/.karabiner.d/ 2> /dev/null
  ln -s $HOME/Developer/scaffold/macos/karabiner.d $HOME/.karabiner.d
fi

if [ ! -L $HOME/.gnupg/gpg-agent.conf ]; then
  mkdir -p $HOME/.gnupg && chmod 700 $HOME/.gnupg
  ln -s $HOME/Developer/scaffold/macos/dotfiles/gpg-agent.conf $HOME/.gnupg/gpg-agent.conf
fi
