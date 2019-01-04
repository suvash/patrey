#!/usr/bin/env bash

# MACOS-only stuff. Abort if not MACOS.
[[ "$OSTYPE" =~ ^darwin ]] || return 1

pushd $PWD

# ============= Install brew ====================

if [[ "$OSTYPE" =~ ^darwin ]] && [[ ! "$(type -P brew)" ]]; then
    echo "Installing Homebrew"

    #Skip the "Press enter to continue‚Äö√Ñ¬∂" prompt.
    true | ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew doctor
fi

# ============= End brew ========================


# ============= Clone scaffold ==================

git clone https://gitlab.com/suvash/scaffold.git $HOME/Developer/scaffold

cd $HOME/Developer/scaffold/

git remote remove origin
git remote add origin git@gitlab.com:suvash/scaffold.git

# ============= End Clone scaffold ==================

popd
