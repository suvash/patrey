#!/usr/bin/env bash

pushd $PWD

# Set computer name
hostnamectl set-hostname yantra

# Update Apt and install Git
sudo apt-get update
sudo apt-get install -y git


# ============= Clone scaffold ==================

git clone https://github.com/suvash/scaffold.git $HOME/Developer/scaffold

cd $HOME/Developer/scaffold/

git remote remove origin
git remote add origin git@github.com:suvash/scaffold.git

# ============= End Clone scaffold ==================


# ============= Install Nix ====================

curl https://nixos.org/nix/install | sh

# ============= End Nix ====================


# ============= Symlink things====================

source $HOME/Developer/scaffold/common/common_symlinks.sh

source $HOME/Developer/scaffold/linux/linux_symlinks.sh

# ============= End symlink things====================

popd
