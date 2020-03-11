#!/usr/bin/env bash

set -eou pipefail

echo '--Updating apt'
sudo apt update

echo '--Installing apt packages'
awk '!/(^#|^$)/{print $0}' "$HOME/.packages.apt" | xargs sudo apt install --yes

echo '--Autoremoving packages'
sudo apt autoremove -y

if ! $(cat /etc/shells | grep -q '/usr/bin/fish'); then
  echo '--Adding fish to available shells, will need privileges.'
  echo '/usr/bin/fish' | sudo tee -a /etc/shells > /dev/null
fi

if ! $(finger $USER | grep -q '/usr/bin/fish'); then
  echo '--Setting fish as default shell, will need privileges.'
  chsh -s /usr/bin/fish
fi

if ! hash conda; then
    echo '--Installing miniconda'
    wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O /tmp/miniconda.sh
    sudo bash /tmp/miniconda.sh -b -p /opt/miniconda
    sudo ln -sfv /opt/miniconda/etc/profile.d/conda.sh /etc/profile.d/conda.sh
fi

if ! hash nvtop; then
    echo '--Installing nvtop'
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
