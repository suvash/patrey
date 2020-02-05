#!/usr/bin/env bash

pushd $PWD

# Set computer properties
hostnamectl set-hostname prayogshala


# Add ppa graphics drivers repository
sudo add-apt-repository ppa:graphics-drivers/ppa --yes


# Symlinks
source $HOME/Developer/scaffold/prayogshala/prayogshala_symlinks.sh

popd
