{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Hardware =============================================

  # Hostname
  networking.hostName = "yantra"; # Define your hostname.

  # Wireless
  networking.wireless.enable = true;  # Enables wireless.
  networking.wireless.interfaces = [ "wlp2s0" ];

  # Enables users to control via wpa_cli/gui
  networking.wireless.userControlled.enable = true;

  # Sounds & Audio
  hardware.pulseaudio.enable = true;

  # Regional/Locale ======================================

  time.timeZone = "Europe/Stockholm";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Packages =============================================

  # List packages installed in system profile. To search by name, run:
  # -env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget
    curl
    xclip
    tree
    file
    autojump # autolearn jump from cd
    axel # better than wget
    silver-searcher

    htop
    iotop
    iftop
    powertop
    acpi

    pavucontrol
    alsaUtils

    dmenu
    scrot
    firefox
    chromium    
    vlc
    evince
    mplayer

    lilyterm
    fish

    vim
    emacs
    mg # micro gnu emacs
    git
    tig
    vagrant
    weechat
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Flash only on chromium
  nixpkgs.config.firefox.enableAdobeFlash = false;
  nixpkgs.config.chromium.enableAdobeFlash = false;

  # Services =============================================

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Maybe try out KDM instead of slim sometime
  # services.xserver.displayManager.kdm.enable = true;

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

  # Enable virtualbox
  services.virtualboxHost.enable = true;
  # services.virtualboxHost.enableHardening = true;

  # Enable acpi daemon
  services.acpid.enable = true;

  # Users ================================================

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.suvash = {
    name = "suvash";
    createHome = true;
    home = "/home/suvash";
    shell = "/run/current-system/sw/bin/bash";
  };

  # Define members for wheel group
  users.extraGroups.wheel.members = [ "suvash" ];

  # Define members for vboxusers group
  users.extraGroups.vboxusers.members = [ "suvash" ];

  # Programs =============================================

}
