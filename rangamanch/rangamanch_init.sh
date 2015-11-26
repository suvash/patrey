#!/usr/bin/env bash

# OSX-only stuff. Abort if not OSX.
[[ "$OSTYPE" =~ ^darwin ]] || return 1

source $HOME/Developer/scaffold/common/common_symlinks.sh

source $HOME/Developer/scaffold/rangamanch/osx/osx_sanity.sh
source $HOME/Developer/scaffold/rangamanch/rangamanch_symlinks.sh

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

    #Skip the "Press enter to continue‚Ä¶" prompt.
    true | ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew doctor
fi

# ============= End brew ========================

popd

source $HOME/Developer/scaffold/rangamanch/rangamanch_rebuild.sh
source $HOME/Developer/scaffold/rangamanch/osx/karabiner_bootstrap.sh
