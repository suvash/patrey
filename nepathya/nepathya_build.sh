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
brew update
brew upgrade

echo "Brew bundling dependencies"
brew bundle --file=$HOME/Developer/scaffold/nepathya/Brewfile

echo "Brew and cask cleanup"
brew cleanup
brew bundle cleanup --file=$HOME/Developer/scaffold/nepathya/Brewfile #--force

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

# Base16 shell download to config directory
if [ ! -d "$HOME/.config/base16-shell" ]; then
  git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
fi
