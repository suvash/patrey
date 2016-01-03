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

# Install completions for fish shell
mkdir -p $HOME/.config/fish/completions/
wget https://raw.githubusercontent.com/docker/docker/master/contrib/completion/fish/docker.fish \
     -O $HOME/.config/fish/completions/docker.fish

# Install 3rd party things
if [ ! -d /opt/viber/ ]; then
  curl http://download.cdn.viber.com/cdn/desktop/Linux/viber.deb > /tmp/viber.deb
  sudo dpkg -i /tmp/viber.deb
fi

# Nix things
nix-env -i bashmount
