# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Networking
  networking.hostName = "yantra"; # Define your hostname.
  # Wireless
  networking.wireless.enable = true;  # Enables wireless.
  networking.wireless.interfaces = [ "wlp2s0" ];
  # Enables users to control via wpa_cli/gui
  networking.wireless.userControlled.enable = true;

  # Sounds & Audio
  hardware.pulseaudio.enable = true;

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # List packages installed in system profile. To search by name, run:
  # -env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget
    curl
    xclip
    tree

    htop
    iotop
    iftop

    pavucontrol
    alsaUtils

    dmenu
    firefox
    chromium    
    vlc

    fish
    vim
    emacs
    git
    tig
    silver-searcher
    weechat
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Flash only on chromium
  nixpkgs.config.firefox.enableAdobeFlash = false;
  nixpkgs.config.chromium.enableAdobeFlash = false;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;

  # XServer configuration
  services.xserver = {
    enable = true;
    autorun = true;
    layout = "us";
    xkbOptions = "ctrl:nocaps";
    windowManager.xmonad.enable = true;
    windowManager.default = "xmonad";
    desktopManager.xterm.enable = true;
    desktopManager.default = "none";
    displayManager.slim.enable = true; 
  };

  # Enable Touchpad support
  services.xserver.synaptics.enable = true;
  services.xserver.synaptics.twoFingerScroll = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.suvash = {
    name = "suvash";
    createHome = true;
    home = "/home/suvash";
    extraGroups = [ "wheel" ];
    shell = "/run/current-system/sw/bin/bash";
  };

}
