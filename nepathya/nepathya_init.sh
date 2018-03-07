#!/usr/bin/env bash

# Ask for the administrator password upfront
sudo -v

# Set computer name (as done via System Preferences ‚Üí Sharing)
sudo scutil --set ComputerName "nepathya"
sudo scutil --set HostName "nepathya"
sudo scutil --set LocalHostName "nepathya"
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string "nepathya"

mkdir -p $HOME/.local/bin

source $HOME/Developer/scaffold/common/common_symlinks.sh

source $HOME/Developer/scaffold/nepathya/nepathya_symlinks.sh
