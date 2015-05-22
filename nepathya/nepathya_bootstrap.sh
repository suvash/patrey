#!/usr/bin/env bash

# OSX-only stuff. Abort if not OSX.
[[ "$OSTYPE" =~ ^darwin ]] || return 1

# Run common_symlinks

pushd $PWD

# ============= Install brew ====================

if [[ "$OSTYPE" =~ ^darwin ]] && [[ ! "$(type -P brew)" ]]; then
    echo "Installing Homebrew"

    #Skip the "Press enter to continue…" prompt.
    true | ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

echo "Creating $HOME/Applications"
mkdir -p $HOME/Applications

echo "Brew bundle"
brew doctor
brew tap homebrew/boneyard

echo "Brew bundling dependencies"
brew bundle $HOME/Developer/scaffold/nepathya/osx/Brewfile

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
