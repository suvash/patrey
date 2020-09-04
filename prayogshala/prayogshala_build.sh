#!/usr/bin/env bash

set -eou pipefail

echo '--Updating apt'
sudo apt update

echo '--Installing apt packages'
awk '!/(^#|^$)/{print $0}' "$HOME/.packages.apt" | xargs sudo apt install --yes

echo '--Autoremoving packages'
sudo apt autoremove -y

if ! grep -q '/usr/bin/fish' < /etc/shells; then
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

if [ ! -d "$HOME/.config/base16-shell" ]; then
  git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
fi
