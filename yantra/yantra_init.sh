#!/usr/bin/env bash

pushd $PWD

# Set computer name
hostnamectl set-hostname yantra

# Update Apt and install Git
sudo apt-get update
sudo apt-get install -y git

# ============= Clone scaffold ==================

if [ ! -d $HOME/Developer/scaffold/ ]; then
  git clone https://github.com/suvash/scaffold.git $HOME/Developer/scaffold

  cd $HOME/Developer/scaffold/

  git remote remove origin
  git remote add origin git@github.com:suvash/scaffold.git
fi

# ============= End Clone scaffold ==================


# ============= Install Nix ====================

if [ ! -d /nix/ ]; then
  curl https://nixos.org/nix/install | sh
fi

# ============= End Nix ====================

# ============= Add Apt Repositories ====================

DISTRIB_CODENAME=$(lsb_release -c | awk '{print $2}')

# Docker repo
sudo apt-key adv --keyserver hkp://p80.pool.sks-keyservers.net:80 --recv-keys 58118E89F3A912897C070ADBF76221572C52609D
echo "deb https://apt.dockerproject.org/repo ubuntu-$DISTRIB_CODENAME main" \
     | sudo tee /etc/apt/sources.list.d/docker.list

# R package
sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
echo "deb http://ftp.acc.umu.se/mirror/CRAN/bin/linux/ubuntu $DISTRIB_CODENAME/" \
     | sudo tee /etc/apt/sources.list.d/r.list


sudo apt-get update

# ============= End Apt Repositories ====================

# ============= Sudoless Docker ====================

sudo usermod -aG docker $(whoami)

# ============= Docker things ====================

# ============= Symlink things====================

source $HOME/Developer/scaffold/common/common_symlinks.sh

source $HOME/Developer/scaffold/linux/linux_symlinks.sh

# ============= End symlink things====================

popd
