#!/usr/bin/env bash

pushd $PWD

# Set computer name
hostnamectl set-hostname yantra

# Update Apt and install Git
sudo apt-get update
sudo apt-get install -y git

# Make sure to get tilde on apple keyboards
MODPROBE_FILE=/etc/modprobe.d/hid_apple.conf
ISO_LAYOUT_OPTION='options hid_apple iso_layout=0'
if [ ! -f $MODPROBE_FILE ]; then
  echo $ISO_LAYOUT_OPTION | sudo tee $MODPROBE_FILE
else
  if ! grep -Fxq "$ISO_LAYOUT_OPTION" $MODPROBE_FILE; then
    echo $ISO_LAYOUT_OPTION | sudo tee -a $MODPROBE_FILE
  fi
fi

# ============= Clone scaffold ==================

git clone https://github.com/suvash/scaffold.git $HOME/Developer/scaffold

cd $HOME/Developer/scaffold/

git remote remove origin
git remote add origin git@github.com:suvash/scaffold.git

# ============= End Clone scaffold ==================


# ============= Install Nix ====================

curl https://nixos.org/nix/install | sh

# ============= End Nix ====================

# ============= Add Tor project repo ====================

sudo tee -a /etc/apt/sources.list > /dev/null <<EOF

## Tor repository
deb http://deb.torproject.org/torproject.org $(lsb_release -c | awk '{print $2}') main
deb-src http://deb.torproject.org/torproject.org $(lsb_release -c | awk '{print $2}') main
EOF

gpg --keyserver keys.gnupg.net --recv 886DDD89
gpg --export A3C4F0F979CAA22CDBA8F512EE8CBC9E886DDD89 | sudo apt-key add -

sudo apt-get update

# ============= End Tor ====================


# ============= Symlink things====================

source $HOME/Developer/scaffold/common/common_symlinks.sh

source $HOME/Developer/scaffold/linux/linux_symlinks.sh

# ============= End symlink things====================

popd
