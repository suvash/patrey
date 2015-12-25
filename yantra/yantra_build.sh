#!/usr/bin/env bash

# Install essentials for window manager
sudo apt-get install -y xmonad dmenu suckless-tools xmobar stalonetray compton

# Install more essentials
sudo apt-get install -y feh scrot termite xscreensaver

# Install power/volume/network nicities
sudo apt-get install -y xfce4-power-manager xfce4-volumed
sudo apt-get install -y nm-applet
sudo apt-get install -y pasystray paman paprefs pavucontrol pavumeter

# Install NFS things so we can use it for vagrant
sudo apt-get install -y nfs-common nfs-kernel-server

