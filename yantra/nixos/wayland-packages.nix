{ pkgs }:

with pkgs;

[
  # X but used for Sway packages
  # Swaybar
  unstable.i3status-rust

  # Fonts
  gnome2.pango

  # X but used for Sway packages END

  # Terminal
  unstable.alacritty

  # Launcher
  unstable.bemenu

  # Brightness
  brightnessctl

  # Clipboard
  wl-clipboard

  # Notification
  unstable.mako
]
