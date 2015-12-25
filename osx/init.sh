#!/usr/bin/env bash

# OSX-only stuff. Abort if not OSX.
[[ "$OSTYPE" =~ ^darwin ]] || return 1

pushd $PWD

# ============= Install Xcode Tools ====================

# create the placeholder file that's checked by CLI updates' .dist code
# in Apple's SUS catalog
touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress
# find the CLI Tools update
PROD=$(softwareupdate -l | grep "\*.*Command Line" | head -n 1 | awk -F"*" '{print $2}' | sed -e 's/^ *//' | tr -d '\n')
# install it
softwareupdate -i "$PROD" -v

# ============= End Xcode tools ========================


# ============= Install brew ====================

if [[ "$OSTYPE" =~ ^darwin ]] && [[ ! "$(type -P brew)" ]]; then
    echo "Installing Homebrew"

    #Skip the "Press enter to continue‚Äö√Ñ¬∂" prompt.
    true | ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew doctor
fi

# ============= End brew ========================

# ============= Install Nix ====================

curl https://nixos.org/nix/install | sh

# ============= End Nix ====================

# ============= Clone scaffold ==================

git clone https://github.com/suvash/scaffold.git $HOME/Developer/scaffold

cd $HOME/Developer/scaffold/

git remote remove origin
git remote add origin git@github.com:suvash/scaffold.git

# ============= End Clone scaffold ==================

popd
