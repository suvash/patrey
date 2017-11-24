#!/usr/bin/env bash

pushd $PWD

# Set computer name
hostnamectl set-hostname rangamanch

# Update Apt and install Git
sudo apt-get update
sudo apt-get install -y git

# ============= Clone scaffold ==================

if [ ! -d $HOME/Developer/scaffold/ ]; then
  git clone https://gitlab.com/suvash/scaffold.git $HOME/Developer/scaffold

  cd $HOME/Developer/scaffold/

  git remote remove origin
  git remote add origin git@gitlab.com:suvash/scaffold.git
fi

# ============= End Clone scaffold ==================

# ============= Local bindir creation ===============

mkdir -p $HOME/.local/bin

# ============= Add Apt Repositories ====================

DISTRIB_CODENAME=$(lsb_release -sc)

# Docker repo
wget -qO- https://download.docker.com/linux/debian/gpg | sudo apt-key add -
echo "deb [arch=amd64] https://download.docker.com/linux/debian $DISTRIB_CODENAME stable" \
     | sudo tee /etc/apt/sources.list.d/docker.list

# Google Cloud SDK repo
wget -qO- https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key add -
echo "deb http://packages.cloud.google.com/apt cloud-sdk-$DISTRIB_CODENAME main" \
     | sudo tee /etc/apt/sources.list.d/google-cloud-sdk.list

# Spotify Client
echo "deb http://repository.spotify.com stable non-free" \
    | sudo tee /etc/apt/sources.list.d/spotify.list

# Virtualbox Repo
wget -qO- https://www.virtualbox.org/download/oracle_vbox_2016.asc | sudo apt-key add -
echo "deb http://download.virtualbox.org/virtualbox/debian $DISTRIB_CODENAME contrib" \
    | sudo tee /etc/apt/sources.list.d/oracle-virtualbox.list

# Google Chrome Repo
wget -qO- https://dl.google.com/linux/linux_signing_key.pub | sudo apt-key add -
echo "deb http://dl.google.com/linux/chrome/deb/ stable main" \
    | sudo tee /etc/apt/sources.list.d/google-chrome.list

sudo apt-get update

# ============= End Apt Repositories ====================

# ============= Sudoless Docker ====================

getent group docker &> /dev/null || sudo groupadd docker
sudo usermod -aG docker $(whoami)

# ============= Docker things ====================

# ============= Symlink things====================

source $HOME/Developer/scaffold/common/common_symlinks.sh

source $HOME/Developer/scaffold/linux/linux_symlinks.sh

source $HOME/Developer/scaffold/rangamanch/rangamanch_symlinks.sh

# ============= End symlink things====================

popd

