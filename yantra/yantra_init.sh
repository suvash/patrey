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
  sudo update-initramfs -u -k all
else
  if ! grep -Fxq "$ISO_LAYOUT_OPTION" $MODPROBE_FILE; then
    echo $ISO_LAYOUT_OPTION | sudo tee -a $MODPROBE_FILE
    sudo update-initramfs -u -k all
  fi
fi

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

# Spotify Client
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys BBEBDCB318AD50EC6865090613B00F1FD2C19886
echo "deb http://repository.spotify.com testing non-free" | sudo tee /etc/apt/sources.list.d/spotify.list

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
