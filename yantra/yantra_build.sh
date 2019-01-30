#!/usr/bin/env bash

# Update channels
# sudo nix-channel --update nixos
# sudo nix-channel --update nixos-unstable

# Use the new configuration
sudo nixos-rebuild switch

# Use local packages
LOCAL_REPO="$HOME/Developer/nixpkgs"
LOCAL_REPO_PACKAGES=()

for package in "${LOCAL_REPO_PACKAGES[@]}"; do
  if ! command -v "$package"; then
    nix-env -f "$LOCAL_REPO" -iA "$package"
  fi
done

# nix-env -q --installed
# nix-env -e docker-credential-gcr

# Collect garbage
sudo nix-collect-garbage
