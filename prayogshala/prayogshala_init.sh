#!/usr/bin/env bash

pushd $PWD

# Set computer properties
hostnamectl set-hostname prayogshala

# Add ppa graphics drivers repository
sudo add-apt-repository ppa:graphics-drivers/ppa --yes

# Add regolith ppa
sudo add-apt-repository ppa:regolith-linux/release --yes

# Add Spotify repository
if [ ! -f /etc/apt/sources.list.d/spotify.list ]; then
  curl -sS https://download.spotify.com/debian/pubkey.gpg | sudo apt-key add -
  echo "deb http://repository.spotify.com stable non-free" | sudo tee /etc/apt/sources.list.d/spotify.list
  sudo apt update
fi

# Ubuntu drivers
sudo apt install ubuntu-drivers-common --yes
echo "Check the recommendation below..."
ubuntu-drivers devices

# Nvidia driver
# sudo apt install nvidia-driver-435 nvidia-utils-435 --yes

# Set $WORK
if ! grep -q 'WORK' < /etc/environment; then
  echo '--Adding $WORK to environment.'
  sudo mkdir -p /work
  echo 'WORK="/work"' | sudo tee -a /etc/environment > /dev/null
fi

# Symlinks
source $HOME/Developer/scaffold/prayogshala/prayogshala_symlinks.sh

popd
