#!/usr/bin/env bash

if [ ! -L $HOME/.nixpkgs ]; then
  rm -rf $HOME/.nixpkgs 2> /dev/null
  ln -s $HOME/Developer/scaffold/yantra/nixpkgs $HOME/.nixpkgs
fi
