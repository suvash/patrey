#!/usr/bin/env bash

# Install all the packages mentioned in packages.list
cat $HOME/.packages.apt | awk '!/(^#|^$)/{print $0}' | xargs sudo apt-get install -y

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
