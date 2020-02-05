#!/usr/bin/env bash

# Update Apt
sudo apt update

# Install all the packages mentioned in packages.apt
cat "$HOME/.packages.apt" | awk '!/(^#|^$)/{print $0}' | xargs sudo apt install --yes

# Autoremove crap
sudo apt autoremove -y

# Install miniconda
if ! hash autorandr; then
    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /tmp/miniconda.sh
    sudo bash /tmp/miniconda.sh -b -p /opt/miniconda
    sudo ln -sfv /opt/miniconda/etc/profile.d/conda.sh /etc/profile.d/conda.sh
fi
