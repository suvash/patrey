#!/usr/bin/env bash

# OSX-only stuff. Abort if not OSX.
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
brew bundle --file=$HOME/Developer/scaffold/rangamanch/Brewfile

echo "Brew linking"
brew linkapps --local

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

popd
