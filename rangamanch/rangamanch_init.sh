#!/usr/bin/env bash

# Ask for the administrator password upfront
sudo -v

# Set computer name (as done via System Preferences ‚Üí Sharing)
sudo scutil --set ComputerName "rangamanch"
sudo scutil --set HostName "rangamanch"
sudo scutil --set LocalHostName "rangamanch"
sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string "rangamanch"

source $HOME/Developer/scaffold/common/common_symlinks.sh

source $HOME/Developer/scaffold/rangamanch/rangamanch_symlinks.sh
