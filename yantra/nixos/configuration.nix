{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./gpg-yubikey-configuration.nix
    ];

  # Boot =================================================

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Kernel parameters
  # https://wiki.archlinux.org/index.php/Dell_XPS_13_(9360)#Module-based_Powersaving_Options
  # https://wiki.archlinux.org/index.php/Dell_XPS_13_(9360)#NVME_Power_Saving_Patch
  boot.kernelParams = [
    "modeset=1"
    "i915.enable_fbc=1"
    "i915.enable_rc6=1"
    "i915.enable_guc_loading=1"
    "i915.enable_guc_submission=1"
    "i915.enable_psr=0"
    "nvme_core.default_ps_max_latency_us=170000"
  ];

  # Mount luksFormat at boot time
  boot.initrd.luks.devices = [
    {
    name = "nixos";
    device = "/dev/nvme0n1p2";
    preLVM = true;
    }
  ];

  # Clean tmp dir on boot
  boot.cleanTmpDir = true;

  # Filesystem Options ===================================

  # Better(?) for SSD disks running on ext4 fs
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  # Networking ===========================================

  # Define your hostname.
  networking.hostName = "yantra";

  # Use networkmanger (instead of wpa_supplicant)
  networking.networkmanager.enable = true;

  # Insert nameservers before DHCP
  networking.networkmanager.insertNameservers = [ "1.1.1.1" "1.0.0.1" ];

  # Firewall
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 7531 ];
  networking.firewall.allowedUDPPorts = [ 7531 ];
  # Or disable the firewall altogether.

  # Power management =====================================

  # Power management
  powerManagement.enable = true;

  # Hardware =============================================

  # CPU Microcode update
  hardware.cpu.intel.updateMicrocode = true;

  # OpenGL
  hardware.opengl.enable = true;

  # PulseAudio
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  # Bluetooth
  hardware.bluetooth.enable = false;

  # Localisation =========================================

  # Select internationalisation properties.
  i18n = {
    consoleFont = "latarcyrheb-sun32";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Nix ==================================================

  # Build in sandbox
  nix.useSandbox = true;

  # Use more cores and run more jobs
  nix.buildCores = 3;
  nix.maxJobs = 3;

  # Packages =============================================

  # Allow unfree packages
  nixpkgs.config = {
    # Allow proprietary packages
    allowUnfree = true;

    # Alias for the unstable channel
    packageOverrides = pkgs: {
      unstable = import <nixos-unstable> {
        # pass the nixpkgs config to the unstable alias
        # to ensure `allowUnfree = true;` is propagated:
        config = config.nixpkgs.config;
      };
    };
  };

  # Environment ==========================================

  # Set env vars
  environment.variables = {
    EDITOR = "vim";
    BROWSER = "firefox";
    _JAVA_AWT_WM_NONREPARENTING = "1"; # Java + Xmonad
    PATH = "$HOME/.local/bin:$PATH";
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    gitFull
    htop
    acpi

    xcape
    redshift

    unstable.zathura

    xscreensaver

    gnumake3

    dmenu
    silver-searcher
    rofi
    tree
    dfc
    arandr

    xclip
    autocutsel

    baobab

    dunst
    libnotify
    stalonetray
    pasystray

    # XPS tools
    nvme-cli
    # unstable.fwupd
    # Fix freezing after waking up from suspend
    # https://wiki.archlinux.org/index.php/Dell_XPS_13_(9360)#Freezing_after_waking_from_suspend
    xorg.xf86videointel


    # Bluetooth
    # blueman

    ranger
    unstable.pulsemixer

    lshw
    hardinfo

    iotop
    iftop
    dfc

    scrot

    unstable.udiskie

    tmux
    unstable.fish
    lilyterm
    emacs
    jq
    unzip

    bind # for dns utils

    anki

    alacritty

    unstable.awscli
    unstable.google-cloud-sdk

    google-chrome
    unstable.opera
    unstable.spotify
    unstable.mendeley

    slock

    multitail

    #pass
    unstable.gopass
    qtpass
    rofi-pass
    python36Packages.upass

    pavucontrol
    paprefs

    cmatrix

    xorg.xev
    xorg.xbacklight

    unstable.terraform
    unstable.packer
    unstable.kubectl
    unstable.kubernetes-helm
    unstable.hugo
    unstable.sops
    unstable.docker-credential-gcr
    unstable.docker_compose

    vnstat

    unstable.cargo

    firefox
    networkmanagerapplet
    nox

    gnome3.cheese

    xfce.xfce4volumed

    haskellPackages.xmobar

    nix-prefetch-scripts
    go2nix

    # Python pipenv
    unstable.pipenv

    # For nm-applet
    hicolor_icon_theme
  ];

  # Fonts ================================================

  fonts.fonts = with pkgs; [
    mononoki
    lohit-fonts.devanagari
    lohit-fonts.nepali
  ];

  # Programs =============================================

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.slock.enable = true;
  # programs.bash.enableCompletion = true;
  # programs.mtr.enable = true;

  # Virtualisation =======================================

  virtualisation.docker.enable = true;
  virtualisation.docker.enableOnBoot = true;

  # Services =============================================

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable acpi daemon
  services.acpid.enable = true;

  # Upower daemon - Culprit
  services.upower.enable = true;

  # Avahi daemon
  services.avahi.enable = true;

  # Dbus service
  services.dbus.enable = true;

  # Vnstat
  services.vnstat.enable = true;

  # Fwupd (Not in stable yet)
  # services.hardware.fwupd.enable = true;

  # Enable SSD TRIM of mounted supported partition in background
  services.fstrim.enable = true;

  services.logind.extraConfig = ''
    HandlePowerKey=suspend
    HandleLidSwitch=suspend
  '';

  # Printing with CUPS
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip ];

  # Autorandr
  services.autorandr.enable = true;

  # Unclutter
  services.unclutter-xfixes.enable = true;

  # Compton
  services.compton.enable = true;
  services.compton.backend = "xrender";
  services.compton.activeOpacity = "1.0";
  services.compton.inactiveOpacity = "0.8";

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.autorun = true;

  services.xserver.layout = "us,us,se";
  services.xserver.xkbVariant = "dvorak,,";
  services.xserver.xkbOptions = "grp:shifts_toggle,ctrl:nocaps";

  # Pick an intel driver for this machine
  services.xserver.videoDrivers = [ "intel" "vesa" ];

  # Enable touchpad support.
  services.xserver.libinput.enable = true;
  services.xserver.libinput.clickMethod = "clickfinger";

  # Select a display manager (login manager)
  services.xserver.displayManager.lightdm.enable = true;

  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.bash}/bin/bash $HOME/.fehbg
    ${pkgs.xorg.xset}/bin/xset r rate 220 60
  '';

  # Select a desktop manager
  services.xserver.desktopManager.default = "none";

  # Select a window manager
  services.xserver.windowManager.default = "xmonad";
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

  # Redshift
  services.redshift.enable = true;
  services.redshift.latitude = "57.5";
  services.redshift.longitude = "12";
  services.redshift.temperature.day = 5500;
  services.redshift.temperature.night = 3700;

  # Systemd ==============================================

  systemd.user.services."xcape" = {
    enable = true;
    description = "xcape : use CTRL as ESC when pressed alone";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.xcape}/bin/xcape";
  };

  systemd.user.services."dunst" = {
    enable = true;
    description = "Dunst : Lightweight and customizable notification daemon";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
  };

  systemd.user.services."xscreensaver" = {
    enable = true;
    description = "X screensaver";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.xscreensaver}/bin/xscreensaver -no-splash";
  };

  systemd.user.services."stalonetray" = {
    enable = true;
    description = "Standalone tray";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.stalonetray}/bin/stalonetray";
  };

   systemd.user.services."autocutsel" = {
    enable = true;
    description = "AutoCutSel tracks changes in various clipboard buffers";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStartPre = "${pkgs.autocutsel}/bin/autocutsel -fork";
    serviceConfig.ExecStart = "${pkgs.autocutsel}/bin/autocutsel -selection PRIMARY -fork";
  };

   systemd.user.services."pasystray" = {
    enable = true;
    description = "PulseAudio system tray";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    environment.XDG_DATA_DIRS="/run/current-system/sw/share/";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.pasystray}/bin/pasystray";
  };

   systemd.user.services."nm-applet" = {
    enable = true;
    description = "Network Manager applet system tray";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    environment.XDG_DATA_DIRS="/run/current-system/sw/share/";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.networkmanagerapplet}/bin/nm-applet";
  };

   systemd.user.services."xfce4-volumed" = {
    enable = true;
    description = "Xfce4 Volume keys control daemon";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.xfce.xfce4volumed}/bin/xfce4-volumed";
  };

   systemd.user.services."udiskie" = {
    enable = true;
    description = "Udiskie : Automounter for removable media";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.unstable.udiskie}/bin/udiskie -A -n -s -F";
  };

  # Users ================================================

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.suvash = {
    isNormalUser = true;
    createHome = true;
    extraGroups = ["wheel" "networkmanager" "docker"];
    shell = "/run/current-system/sw/bin/fish";
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.03";

}
