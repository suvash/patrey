#!/usr/bin/env bash

# Install essentials for window manager
sudo apt-get install -y xmonad dmenu suckless-tools xmobar stalonetray compton seahorse

# Install more essentials
sudo apt-get install -y feh scrot finger tmux fortune

# Install screensaver things
sudo apt-get install -y xscreensaver xscreensaver-data xscreensaver-data-extra xscreensaver-gl xscreensaver-gl-extra xscreensaver-screensaver-dizzy xscreensaver-screensaver-bsod xscreensaver-screensaver-webcollage unicode-screensaver

# Install power/volume/network nicities
sudo apt-get install -y xfce4-power-manager xfce4-volumed
sudo apt-get install -y nm-applet
sudo apt-get install -y pasystray paman paprefs pavucontrol pavumeter

# Install NFS things so we can use it for vagrant
sudo apt-get install -y nfs-common nfs-kernel-server vagrant virtualbox

# Install other essentials
sudo apt-get install -y htop iotop iftop acpi ncdu dfc strace

sudo apt-get install -y lilyterm feh arandr chromium-browser fish

# Install the editors !
sudo apt-get install -y vim emacs24 silversearcher-ag tmux jq git tig

# Install utilities
sudo apt-get install -y xclip xcape tree unzip unrar unclutter vlc zathura baobab ranger

# Autoremove crap
sudo apt-get autoremove -y

# Add fish to shell list if not already
if ! $(cat /etc/shells | grep -q '/usr/bin/fish'); then
  echo 'Adding fish to available shells, will need privileges.'

  echo '/usr/bin/fish' | sudo tee -a /etc/shells > /dev/null
fi

# Make fish default shell
if ! $(finger $USER | grep -q '/usr/bin/fish'); then
  echo 'Setting fish as default shell, will need privileges.'

  chsh -s /usr/bin/fish
fi

# Nix things
nix-env -i bashmount
