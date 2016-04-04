#!/usr/bin/env bash

# Install all the packages mentioned in packages.list
cat $HOME/.packages.apt | awk '!/(^#|^$)/{print $0}' | xargs sudo apt-get install --yes --force-yes

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
if [ ! -f $HOME/.config/fish/completions/docker.fish ]; then
  mkdir -p $HOME/.config/fish/completions/
  wget https://raw.githubusercontent.com/docker/docker/master/contrib/completion/fish/docker.fish \
       -O $HOME/.config/fish/completions/docker.fish
fi

# Install 3rd party things

# Viber
if [ ! -d /opt/viber/ ]; then
  curl http://download.cdn.viber.com/cdn/desktop/Linux/viber.deb > /tmp/viber.deb
  sudo dpkg -i /tmp/viber.deb
fi

# Synology cloud station
if ! hash synology-cloud-station; then
  curl https://global.download.synology.com/download/Tools/CloudStationDrive/4.0-4203/Ubuntu/Installer/x86_64/synology-cloud-station-drive-4203.x86_64.deb > /tmp/synology.deb
  sudo dpkg -i /tmp/synology.deb
fi

# Tor browser
# https://www.torproject.org/

# Below two things will have to go in data analysis box
# and be ssh X forwarded eventually
# R studio
# https://www.rstudio.com/products/rstudio/download/

# RapidMiner
# https://rapidminer.com/products/studio/

# Mbpfan - controls fan on macbooks and works quite good
if ! hash mbpfan; then
  pushd $PWD
  mkdir -p $HOME/Developer/utilities/
  git clone https://github.com/dgraziotin/mbpfan.git $HOME/Developer/utilities/mbpfan
  cd $HOME/Developer/utilities/mbpfan
  sudo make install
  sudo cp mbpfan.service /etc/systemd/system/
  sudo systemctl daemon-reload
  sudo systemctl start mbpfan.service
  sudo systemctl enable mbpfan.service
  popd
fi

# Nix things
nix-env -iA nixpkgs.yantra
nix-collect-garbage
