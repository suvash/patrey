#!/usr/bin/env bash

# NEEDS to be before -- Spotify requires libssl1.0.0 ~ so annoyning
if ! hash spotify; then
  curl -L http://ftp.se.debian.org/debian/pool/main/o/openssl/libssl1.0.0_1.0.1t-1+deb8u6_amd64.deb > /tmp/libssl1.0.0.deb
  sudo dpkg -i /tmp/libssl1.0.0.deb && rm /tmp/libssl1.0.0.deb
fi

# Install all the packages mentioned in packages.list
cat $HOME/.packages.apt | awk '!/(^#|^$)/{print $0}' | xargs sudo apt-get install --yes

# Autoremove crap
sudo apt-get autoremove -y

# Add fish to shell list if not already
if ! $(cat /etc/shells | grep -q '/usr/bin/fish'); then
  echo 'Adding fish to available shells, will need privileges.'

  echo '/usr/bin/fish' | sudo tee -a /etc/shells > /dev/null
fi

# Make fish default shell
if ! $(finger $USER | grep -q '/usr/bin/fish'); then
  echo 'Setting fish as default shell, will need privileges.'

  chsh -s /usr/bin/fish
fi

# Install 3rd party things

# Opera
if ! hash opera; then
  curl -L http://download1.operacdn.com/pub/opera/desktop/47.0.2631.55/linux/opera-stable_47.0.2631.55_amd64.deb > /tmp/opera.deb
  sudo dpkg -i /tmp/opera.deb && rm /tmp/opera.deb
fi

# Vagrant
if ! hash vagrant; then
  curl -L https://releases.hashicorp.com/vagrant/1.9.8/vagrant_1.9.8_x86_64.deb > /tmp/vagrant.deb
  sudo dpkg -i /tmp/vagrant.deb && rm /tmp/vagrant.deb
fi

# Terraform
if ! hash terraform; then
  curl -L https://releases.hashicorp.com/terraform/0.10.3/terraform_0.10.3_linux_amd64.zip > /tmp/terraform.zip
  unzip /tmp/terraform.zip -d /tmp && chmod +x /tmp/terraform
  sudo mv /tmp/terraform $HOME/.local/bin/terraform
fi

# Docker compose
if ! hash docker-compose; then
  curl -L https://github.com/docker/compose/releases/download/1.16.1/docker-compose-Linux-x86_64 > /tmp/docker-compose
  chmod +x /tmp/docker-compose
  sudo mv /tmp/docker-compose $HOME/.local/bin/docker-compose
fi

# Ctop
if ! hash ctop; then
  curl -L https://github.com/bcicen/ctop/releases/download/v0.6.1/ctop-0.6.1-linux-amd64 > /tmp/ctop
  chmod +x /tmp/ctop
  sudo mv /tmp/ctop $HOME/.local/bin/ctop
fi

# Skype Alpha for linux
if ! hash skypeforlinux; then
  curl -L https://repo.skype.com/latest/skypeforlinux-64.deb > /tmp/skype.deb
  sudo dpkg -i /tmp/skype.deb
fi
