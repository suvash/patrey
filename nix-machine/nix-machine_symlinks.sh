#!/usr/bin/env bash

if [ ! -L /etc/nixos ]; then
sudo mv /etc/nixos /etc/nixos.orig
sudo ln -s $HOME/Developer/scaffold/nix-machine/nixos/ /etc/nixos
fi
