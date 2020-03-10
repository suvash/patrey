#!/usr/bin/env bash

mkdir -p "$HOME/.local/bin/"
mkdir -p "$HOME/.config/"
mkdir -p "$HOME/.config/autorandr"
mkdir -p "$HOME/.config/nixpkgs"

if [ ! -L /etc/nixos ]; then
  sudo mv /etc/nixos /etc/nixos.orig
  sudo ln -sfnv "$HOME/Developer/scaffold/yantra/nixos/" /etc/nixos
fi

if [ ! -L "$HOME/.Xresources" ]; then
  rm -rf "$HOME/.Xresources" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/dotfiles/Xresources" "$HOME/.Xresources"
fi

if [ ! -L "$HOME/.Xdefaults" ]; then
  rm -rf "$HOME/.Xdefaults" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/dotfiles/Xresources" "$HOME/.Xdefaults"
fi

if [ ! -L "$HOME/.config/nixpkgs/config.nix" ]; then
  rm -rf "$HOME/.config/nixpkgs/config.nix" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/nixpkgs/config.nix" "$HOME/.config/nixpkgs/config.nix"
fi

if [ ! -L "$HOME/.local/bin/autoconfigure-workstation" ]; then
  rm -rf "$HOME/.local/bin/autoconfigure-workstation" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/scripts/autoconfigure-workstation" "$HOME/.local/bin/autoconfigure-workstation"
fi

if [ ! -L "$HOME/.local/bin/utf-demo" ]; then
  rm -rf "$HOME/.local/bin/utf-demo" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/scripts/utf-demo" "$HOME/.local/bin/utf-demo"
fi

if [ ! -L "$HOME/.local/bin/wifi-toggle" ]; then
  rm -rf "$HOME/.local/bin/wifi-toggle" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/scripts/wifi-toggle" "$HOME/.local/bin/wifi-toggle"
fi

if [ ! -L "$HOME/.local/bin/sway-keyboard-layouts" ]; then
  rm -rf "$HOME/.local/bin/sway-keyboard-layouts" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/scripts/sway-keyboard-layouts" "$HOME/.local/bin/sway-keyboard-layouts"
fi

if [ ! -L "$HOME/.local/bin/random-wallpaper" ]; then
  rm -rf "$HOME/.local/bin/random-wallpaper" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/scripts/random-wallpaper" "$HOME/.local/bin/random-wallpaper"
fi

if [ ! -L "$HOME/.xmonad" ]; then
  rm -rf "$HOME/.xmonad" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/xmonad" "$HOME/.xmonad"
fi

if [ ! -L "$HOME/.config/autorandr/preswitch" ]; then
  rm -rf "$HOME/.config/autorandr/preswitch" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/autorandr/preswitch" "$HOME/.config/autorandr/preswitch"
fi

if [ ! -L "$HOME/.config/autorandr/postswitch" ]; then
  rm -rf "$HOME/.config/autorandr/postswitch" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/autorandr/postswitch" "$HOME/.config/autorandr/postswitch"
fi

if [ ! -L "$HOME/.xmobarrc" ]; then
  rm -rf "$HOME/.xmobarrc" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/dotfiles/xmobarrc" "$HOME/.xmobarrc"
fi

if [ ! -L "$HOME/.stalonetrayrc" ]; then
  rm -rf "$HOME/.stalonetrayrc" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/dotfiles/stalonetrayrc" "$HOME/.stalonetrayrc"
fi

if [ ! -L "$HOME/.xscreensaver" ]; then
  rm -rf "$HOME/.xscreensaver" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/dotfiles/xscreensaver" "$HOME/.xscreensaver"
fi

if [ ! -L "$HOME/.config/dunst" ]; then
  rm -rf "$HOME/.config/dunst/" 2> /dev/null
  mkdir -p "$HOME/.config/"
  ln -sfnv "$HOME/Developer/scaffold/yantra/dunst" "$HOME/.config/dunst"
fi

if [ ! -L "$HOME/.config/xfce4" ]; then
  rm -rf "$HOME/.config/xfce4" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/xfce4" "$HOME/.config/xfce4"
fi

if [ ! -L "$HOME/.config/kitty" ]; then
  rm -rf "$HOME/.config/kitty" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/kitty" "$HOME/.config/kitty"
fi

if [ ! -L "$HOME/.config/alacritty" ]; then
  rm -rf "$HOME/.config/alacritty" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/alacritty" "$HOME/.config/alacritty"
fi

if [ ! -L "$HOME/.config/sway" ]; then
  rm -rf "$HOME/.config/sway" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/sway" "$HOME/.config/sway"
fi

if [ ! -L "$HOME/.config/i3status-rs" ]; then
  rm -rf "$HOME/.config/i3status-rs" 2> /dev/null
  ln -sfnv "$HOME/Developer/scaffold/yantra/i3status-rs" "$HOME/.config/i3status-rs"
fi
