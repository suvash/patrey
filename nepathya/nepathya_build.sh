#!/usr/bin/env bash

# MACOS-only stuff. Abort if not MACOS.
[[ "$OSTYPE" =~ ^darwin ]] || return 1

pushd $PWD

echo "Creating $HOME/Applications"
mkdir -p $HOME/Applications

echo "Brew bundle"
brew tap homebrew/bundle

# update and upgrade
echo "Brew update/upgrading"
brew update --rebase
brew upgrade

echo "Brew bundling dependencies"
brew bundle --file=$HOME/Developer/scaffold/nepathya/Brewfile

echo "Brew and cask cleanup"
brew cleanup
brew cask cleanup

# Add fish to shell list if not already
if ! $(cat /etc/shells | grep -q '/usr/local/bin/fish'); then
  echo 'Adding fish to available shells, will need privileges.'

  echo '/usr/local/bin/fish' | sudo tee -a /etc/shells > /dev/null
fi

# Make fish default shell
if ! $(finger $USER | grep -q '/usr/local/bin/fish'); then
  echo 'Setting fish as default shell, will need privileges.'

  chsh -s /usr/local/bin/fish
fi

# ============= End brew ========================

if ! hash docker-credential-gcr; then
  curl -L https://github.com/GoogleCloudPlatform/docker-credential-gcr/releases/download/v1.4.1/docker-credential-gcr_darwin_amd64-1.4.1.zip > /tmp/docker-credential-helper.zip
  unzip /tmp/docker-credential-helper.zip -d /tmp
  chmod +x /tmp/docker-credential-gcr
  mv /tmp/docker-credential-gcr $HOME/.local/bin/docker-credential-gcr
fi

popd
