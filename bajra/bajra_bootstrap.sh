#!/usr/bin/env bash

# OSX-only stuff. Abort if not OSX.
[[ "$OSTYPE" =~ ^darwin ]] || return 1

# Run common_symlinks

pushd $PWD

# ============= Install nix =====================
sudo rm -rf /etc/nix 2> /dev/null
curl https://nixos.org/nix/install | sh
export PATH=$HOME/.nix-profile/bin:$PATH

# For your corresponding shell
# Set PATH to $HOME/.nix-profile/bin:$PATH
# Set NIX_PATH to $HOME/Developer/nixpkgs:nixpkgs=$HOME/Developer/nixpkgs

# Use a darwin nixpkgs repo
git clone https://github.com/joelteon/nixpkgs.git $HOME/Developer/nixpkgs
cd $HOME/Developer/nixpkgs
git fetch origin
git rebase -p origin/master
nix-channel --remove nixpkgs
cd $HOME/.nix-defexpr
rm -rf *
ln -s $HOME/Developer/nixpkgs .
sudo mkdir /etc/nix
echo "# /etc/nix/nix.conf" | sudo tee -a /etc/nix/nix.conf > /dev/null
echo "" | sudo tee -a /etc/nix/nix.conf > /dev/null
echo "binary-caches = http://zalora-public-nix-cache.s3-website-ap-southeast-1.amazonaws.com/ http://cache.nixos.org/" | sudo tee -a /etc/nix/nix.conf > /dev/null
nix-env -iA nixpkgs.nix

# ============= End nix =========================

# ============= Install brew ====================

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
