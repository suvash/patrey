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
    xcape
    xscreensaver
    fortune
    tree
    file
    silver-searcher
    unzip
    unrar
    vifm
    tty-clock

    htop
    iotop
    iftop
    acpi
    ncdu
    dfc
    psmisc

    pavucontrol
    alsaUtils
    pamixer

    # iphone tethering and mounting
    # sudo usbmuxd ; turn on personal hotspot; turn off wifi/wpa-cli; idevicepair pair/unpair
    libimobiledevice
    usbmuxd
    # same as above; unlock screen; ifuse /mount/point; fusermount -u /mount/point
    ifuse

    dmenu
    scrot
    firefox
    chromium
    vlc
    zathura
    mplayer
    dunst
    libnotify
    baobab
    feh

    ruby_2_1_2
    leiningen
    elixir

    lilyterm
    fish
    vim
    # vimPlugins.ctrlp #install this with users own config/cli
    emacs
    vagrant
    weechat

    git
    tig
    gitAndTools.hub

    xlibs.xev
    xlibs.xbacklight
    xlibs.xkill
    xlibs.xclock
    xlibs.xrandr

    gnome3.gnome_icon_theme
    gnome3.gnome_icon_theme_symbolic

    haskellPackages.xmonad
    haskellPackages.xmonadExtras
    haskellPackages.xmonadContrib

    haskellPackages.hledger
    haskellPackages.hledgerWeb
  ];

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # Flash only on chromium
  nixpkgs.config.firefox.enableAdobeFlash = false;
  nixpkgs.config.chromium.enableAdobeFlash = true;

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
    startGnuPGAgent = true;

    # X Keyboard options
    xkbOptions = "ctrl:nocaps";

    # Just use xmonad, what's the problem ?
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    windowManager.default = "xmonad";

    # No need for desktop manager
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";

    # Display/Login manager
    displayManager.slim.enable = true;
    displayManager.slim.theme = pkgs.fetchurl {
      url = "mirror://sourceforge/slim.berlios/slim-wave.tar.gz";
      sha256 = "0ndr419i5myzcylvxb89m9grl2xyq6fbnyc3lkd711mzlmnnfxdy";
    };
    displayManager.sessionCommands = ''
      ${pkgs.xlibs.xset}/bin/xset r rate 200 60              # set the keyboard repeat rate
      ${pkgs.xcape}/bin/xcape                                # use xcape
      ${pkgs.xscreensaver}/bin/xscreensaver -no-splash &     # use xscreensaver
    '';

    # Enable Touchpad support using synaptics driver
    # https://wiki.archlinux.org/index.php/Touchpad_Synaptics
    synaptics.enable = true;
    synaptics.tapButtons = true;
    synaptics.buttonsMap = [ 1 3 2 ];
    synaptics.fingersMap = [ 1 3 2 ];
    synaptics.palmDetect = true;
    synaptics.twoFingerScroll = true;
    synaptics.vertEdgeScroll = false;

    # Natural scrolling
    synaptics.additionalOptions = ''
      Option "VertScrollDelta"  "-111"
      Option "HorizScrollDelta" "-111"
    '';

  };

  # Enable virtualbox
  services.virtualboxHost.enable = true;
  # services.virtualboxHost.enableHardening = true;

  # Enable acpi daemon
  services.acpid.enable = true;
  services.acpid.lidEventCommands = ''
    LID_STATE=/proc/acpi/button/lid/LID0/state
    if [ $(/run/current-system/sw/bin/awk '{print $2}' $LID_STATE) = 'closed' ]; then
      systemctl suspend
    fi
  '';

  # Users ================================================

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.suvash = {
    name = "suvash";
    createHome = true;
    home = "/home/suvash";
    shell = "/run/current-system/sw/bin/fish";
  };

  # Define members for wheel group
  users.extraGroups.wheel.members = [ "suvash" ];

  # Define members for vboxusers group
  users.extraGroups.vboxusers.members = [ "suvash" ];

  # Programs =============================================
  programs.ssh.startAgent = false;
  # programs.ssh.agentTimeout = null;
  # programs.ssh.forwardX11 = false;

}
