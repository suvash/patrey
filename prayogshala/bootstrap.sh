#!/usr/bin/env bash

sudo apt update
sudo apt install -y git

pushd $PWD

if [ ! -d $HOME/Developer/scaffold/ ]; then
  mkdir -p $HOME/Developer
  git clone https://gitlab.com/suvash/scaffold.git $HOME/Developer/scaffold

  cd $HOME/Developer/scaffold/

  git remote remove origin
  git remote add origin git@gitlab.com:suvash/scaffold.git
fi

popd
