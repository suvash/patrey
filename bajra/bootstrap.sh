#!/bin/bash

# OSX-only stuff. Abort if not OSX.
[[ "$OSTYPE" =~ ^darwin ]] || return 1

# Install Homebrew.
if [[ "$OSTYPE" =~ ^darwin ]] && [[ ! "$(type -P brew)" ]]; then
    echo "Installing Homebrew"

    #Skip the "Press enter to continue…" prompt.
    true | ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"
fi

echo "Brew bundle"
brew doctor
brew tap homebrew/boneyard

echo "Brew bundling dependencies"
brew bundle $HOME/Developer/scaffold/bajra/osx/Brewfile

# Add fish to shell list if not already
if ! $(cat /etc/shells | grep -q 'fish'); then
  echo 'Setting fish as default shell, will need sudo privilege'

  echo '/usr/local/bin/fish' | sudo tee -a /etc/shells > /dev/null
fi

# Make fish default shell
chsh -s /usr/local/bin/fish
