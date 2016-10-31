#!/usr/bin/env bash

if [ ! -L $HOME/.tmux-osx.conf ]; then
  rm $HOME/.tmux-osx.conf 2> /dev/null
  ln -s $HOME/Developer/scaffold/osx/tmux-osx.conf $HOME/.tmux-osx.conf
fi

if [ ! -L $HOME/.mackup.cfg ]; then
  rm $HOME/.mackup.cfg 2> /dev/null
  ln -s $HOME/Developer/scaffold/osx/mackup.cfg $HOME/.mackup.cfg
fi

if [ ! -L $HOME/.karabiner.d ]; then
  rm -rf $HOME/.karabiner.d/ 2> /dev/null
  ln -s $HOME/Developer/scaffold/osx/karabiner.d $HOME/.karabiner.d
fi
