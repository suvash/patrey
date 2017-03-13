#!/usr/bin/env bash

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

# Update PCI Ids
sudo update-pciids

# Install 3rd party things

# Vagrant
if ! hash vagrant; then
  curl -L https://releases.hashicorp.com/vagrant/1.9.1/vagrant_1.9.1_x86_64.deb > /tmp/vagrant.deb
  sudo dpkg -i /tmp/vagrant.deb && rm /tmp/vagrant.deb
fi

# Terraform
if ! hash terraform; then
  curl -L https://releases.hashicorp.com/terraform/0.7.10/terraform_0.7.10_linux_amd64.zip > /tmp/terraform.zip
  unzip /tmp/terraform.zip -d /tmp && chmod +x /tmp/terraform
  sudo mv /tmp/terraform /usr/local/bin/terraform
fi

# Docker compose
if ! hash docker-compose; then
  curl -L https://github.com/docker/compose/releases/download/1.10.0/docker-compose-Linux-x86_64 > /tmp/docker-compose
  chmod +x /tmp/docker-compose
  sudo mv /tmp/docker-compose /usr/local/bin/docker-compose
fi

# Docker Machine
if ! hash docker-machine; then
  curl -L https://github.com/docker/machine/releases/download/v0.9.0/docker-machine-Linux-x86_64 > /tmp/docker-machine
  chmod +x /tmp/docker-machine
  sudo mv /tmp/docker-machine /usr/local/bin/docker-machine
fi

# Ctop
if ! hash ctop; then
  curl -L https://github.com/bcicen/ctop/releases/download/v0.4.1/ctop-0.4.1-linux-amd64 > /tmp/ctop
  chmod +x /tmp/ctop
  sudo mv /tmp/ctop /usr/local/bin/ctop
fi

# Skype Alpha for linux
if ! hash skypeforlinux; then
  curl -L https://go.skype.com/skypeforlinux-64-alpha.deb > /tmp/skype.deb
  sudo dpkg -i /tmp/skype.deb
fi

# Synology cloud station
if ! hash synology-cloud-station-drive; then
  curl https://global.download.synology.com/download/Tools/CloudStationDrive/4.2.1-4374/Ubuntu/Installer/x86_64/synology-cloud-station-drive-4374.x86_64.deb > /tmp/synology.deb
  sudo dpkg -i /tmp/synology.deb
fi

# Mbpfan - controls fan on macbooks and works quite good
if ! hash mbpfan; then
  pushd $PWD
  mkdir -p $HOME/Developer/utilities/
  git clone https://github.com/dgraziotin/mbpfan.git $HOME/Developer/utilities/mbpfan
  cd $HOME/Developer/utilities/mbpfan
  sudo make install
  sudo cp mbpfan.service /etc/systemd/system/
  sudo systemctl daemon-reload
  sudo systemctl start mbpfan.service
  sudo systemctl enable mbpfan.service
  popd
fi

# Facetime HD camera
if ! find /lib/modules/$(uname -r) -type f -name \*.ko | grep facetimehd; then
  pushd $PWD
  mkdir -p $HOME/Developer/utilities/
  git clone https://github.com/patjak/bcwc_pcie $HOME/Developer/utilities/facetimehd
  cd $HOME/Developer/utilities/facetimehd/firmware
  make
  sudo make install
  cd $HOME/Developer/utilities/facetimehd/
  make
  sudo make install
  sudo depmod
  popd
fi
