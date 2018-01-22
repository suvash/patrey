#!/usr/bin/env bash

# NEEDS to be before -- Spotify requires libssl1.0.0 ~ so annoyning
if ! hash spotify; then
  curl -L http://ftp.se.debian.org/debian/pool/main/o/openssl/libssl1.0.0_1.0.1t-1+deb8u6_amd64.deb > /tmp/libssl1.0.0.deb
  sudo dpkg -i /tmp/libssl1.0.0.deb && rm /tmp/libssl1.0.0.deb
fi

# Update Apt
sudo apt-get update && sudo apt-get dist-upgrade -y

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

# Backlight
if ! hash xbacklight; then
  git clone https://github.com/wavexx/acpilight.git $HOME/Developer/acpilight
  if [ ! -L /etc/udev/rules.d/90-backlight.rules ]; then
    sudo ln -s $HOME/Developer/acpilight/90-backlight.rules /etc/udev/rules.d/90-backlight.rules
  fi
  ln -s $HOME/Developer/acpilight/xbacklight $HOME/.local/bin/
fi

# NordVPN files
if [ ! -d $HOME/Developer/nordvpn/ ]; then
  curl -L http://downloads.nordcdn.com/configs/archives/servers/ovpn.zip > /tmp/ovpn.zip
  unzip /tmp/ovpn.zip -d $HOME/Developer/nordvpn
  rm /tmp/ovpn.zip
fi

# Autorandr files
if ! hash autorandr; then
  git clone https://github.com/phillipberndt/autorandr.git $HOME/Developer/autorandr
  cd $HOME/Developer/autorandr && make deb && cp autorandr-*.deb autorandr.deb && sudo dpkg -i ./autorandr.deb
  sudo sytemctl daemon-reload
  sudo systemctl enable autorandr.service
fi

# Opera
if ! hash opera; then
  curl -L http://download1.operacdn.com/pub/opera/desktop/47.0.2631.55/linux/opera-stable_47.0.2631.55_amd64.deb > /tmp/opera.deb
  sudo dpkg -i /tmp/opera.deb && rm /tmp/opera.deb
fi

# Tor browser
if ! hash torbrowser-launcher; then
  curl -L  http://ftp.se.debian.org/debian/pool/contrib/t/torbrowser-launcher/torbrowser-launcher_0.2.7-3_amd64.deb > /tmp/tor-browser.deb
  sudo gdebi -n /tmp/tor-browser.deb && rm /tmp/tor-browser.deb
fi

# Vagrant
if ! hash vagrant; then
  curl -L https://releases.hashicorp.com/vagrant/1.9.8/vagrant_1.9.8_x86_64.deb > /tmp/vagrant.deb
  sudo dpkg -i /tmp/vagrant.deb && rm /tmp/vagrant.deb
fi

# Sops
if ! hash sops; then
  curl -L https://github.com/mozilla/sops/releases/download/3.0.0/sops-3.0.0.linux > $HOME/.local/bin/sops
  chmod +x $HOME/.local/bin/sops
fi


# Terraform
if ! hash terraform; then
  curl -L https://releases.hashicorp.com/terraform/0.11.0/terraform_0.11.0_linux_amd64.zip > /tmp/terraform.zip
  unzip /tmp/terraform.zip -d /tmp && chmod +x /tmp/terraform
  mv /tmp/terraform $HOME/.local/bin/terraform
fi

# Docker compose
if ! hash docker-compose; then
  curl -L https://github.com/docker/compose/releases/download/1.17.0/docker-compose-Linux-x86_64 > /tmp/docker-compose
  chmod +x /tmp/docker-compose
  mv /tmp/docker-compose $HOME/.local/bin/docker-compose
fi

# Ctop
if ! hash ctop; then
  curl -L https://github.com/bcicen/ctop/releases/download/v0.6.1/ctop-0.6.1-linux-amd64 > /tmp/ctop
  chmod +x /tmp/ctop
  mv /tmp/ctop $HOME/.local/bin/ctop
fi

# Skype Alpha for linux
if ! hash skypeforlinux; then
  curl -L https://repo.skype.com/latest/skypeforlinux-64.deb > /tmp/skype.deb
  sudo dpkg -i /tmp/skype.deb && rm /tmp/skype.deb
fi

# Docker Credential helper for GCR
if ! hash docker-credential-gcr; then
  curl -L https://github.com/GoogleCloudPlatform/docker-credential-gcr/releases/download/v1.4.1/docker-credential-gcr_linux_amd64-1.4.1.tar.gz > /tmp/docker-credential-helper.tar.gz
  tar -xzf /tmp/docker-credential-helper.tar.gz --directory /tmp/
  chmod +x /tmp/docker-credential-gcr
  mv /tmp/docker-credential-gcr $HOME/.local/bin/docker-credential-gcr && rm /tmp/docker-credential-helper.tar.gz
fi

# Kubectx
if ! hash kubectx; then
  curl -L https://raw.githubusercontent.com/ahmetb/kubectx/master/kubectx > $HOME/.local/bin/kubectx
  chmod +x $HOME/.local/bin/kubectx
  curl -L https://raw.githubusercontent.com/ahmetb/kubectx/master/kubens > $HOME/.local/bin/kubens
  chmod +x $HOME/.local/bin/kubens
  curl -L https://raw.githubusercontent.com/ahmetb/kubectx/master/utils.bash > $HOME/.local/bin/utils.bash
  curl -L https://raw.githubusercontent.com/ahmetb/kubectx/master/completion/kubectx.fish > $HOME/.config/fish/completions/kubectx.fish
  curl -L https://raw.githubusercontent.com/ahmetb/kubectx/master/completion/kubens.fish > $HOME/.config/fish/completions/kubens.fish
fi

# Go Hugo
if ! hash hugo; then
  curl -L https://github.com/gohugoio/hugo/releases/download/v0.29/hugo_0.29_Linux-64bit.tar.gz > /tmp/hugo.tar.gz
  tar -xzf /tmp/hugo.tar.gz --directory /tmp/
  chmod +x /tmp/hugo
  mv /tmp/hugo $HOME/.local/bin/hugo && rm /tmp/hugo.tar.gz
fi

# Mononoki font
if ! fc-list | grep mononoki > /dev/null; then
  curl -L https://github.com/madmalik/mononoki/releases/download/1.2/mononoki.zip > /tmp/mononoki.zip
  mkdir -p $HOME/.local/share/fonts/truetype/mononoki
  unzip -d $HOME/.local/share/fonts/truetype/mononoki /tmp/mononoki.zip && rm /tmp/mononoki.zip
  fc-cache
fi
