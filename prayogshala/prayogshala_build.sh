#!/usr/bin/env bash

# Update Apt
sudo apt update

# Install all the packages mentioned in packages.apt
awk '!/(^#|^$)/{print $0}' "$HOME/.packages.apt" | xargs sudo apt install --yes

# Autoremove crap
sudo apt autoremove -y

# Install miniconda
if ! hash conda; then
    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /tmp/miniconda.sh
    sudo bash /tmp/miniconda.sh -b -p /opt/miniconda
    sudo ln -sfv /opt/miniconda/etc/profile.d/conda.sh /etc/profile.d/conda.sh
fi

if ! hash nvtop; then
    pushd $PWD
    git clone https://github.com/Syllo/nvtop.git /tmp/nvtop
    mkdir -p /tmp/nvtop/build && cd /tmp/nvtop/build
    cmake .. -DNVML_RETRIEVE_HEADER_ONLINE=True
    make
    sudo make install
    cd
    rm -rf /tmp/nvtop
    popd
fi
