{ pkgs }:

with pkgs;

[
  # X but used for Sway packages
  # Swaybar
  i3status

  # Fonts
  gnome2.pango

  # X but used for Sway packages END

  # Brightness
  brightnessctl

  # Clipboard
  wl-clipboard

  # Notification
  unstable.mako
]
