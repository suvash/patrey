#!/usr/bin/env bash

if [ ! -L $HOME/.packages.apt ]; then
  rm -rf "$HOME/.packages.apt" 2> /dev/null
  ln -s "$HOME/Developer/scaffold/prayogshala/packages.apt" "$HOME/.packages.apt"
fi
