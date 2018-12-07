#!/usr/bin/env bash

# Update channels
# sudo nix-channel --update nixos
# sudo nix-channel --update nixos-unstable

# Use the new configuration
sudo nixos-rebuild switch

# Downloads scripts/packages and add to local bin
KUBECTX_VERSION="0.6.2"
if [ ! -e $HOME/.local/bin/kubectx ]; then
  wget --quiet -O "/tmp/kubectx.tar.gz" "https://github.com/ahmetb/kubectx/archive/v$KUBECTX_VERSION.tar.gz" \
  && cd "/tmp" && tar xzf "/tmp/kubectx.tar.gz" \
  && cp /tmp/kubectx-$KUBECTX_VERSION/kube* $HOME/.local/bin/ \
  && cp /tmp/kubectx-$KUBECTX_VERSION/completion/*.fish $HOME/.config/fish/completions/ \
  && cd - && rm -rf /tmp/kubectx-$KUBECTX_VERSION/
fi

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
