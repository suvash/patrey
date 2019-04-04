{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix

      # Hardware configuration
      ./hardware/yubikey.nix

      # Local Services configuration
      ./services/gpg.nix

      # System user services
      ./services/systemd/user/udiskie.nix
      ./services/systemd/user/xfce4-volumed.nix
      ./services/systemd/user/nm-applet.nix
      ./services/systemd/user/pasystray.nix
      ./services/systemd/user/autocutsel.nix
      ./services/systemd/user/stalonetray.nix
      ./services/systemd/user/xscreensaver.nix
      ./services/systemd/user/dunst.nix
      ./services/systemd/user/xcape.nix
    ];

  # Boot =================================================

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Use the latest stable kernel
  boot.kernelPackages = pkgs.linuxPackages_latest;

  # Use kernel modules
  boot.kernelModules = [
    "fuse"
  ];

  # Use extra kernel modules
  boot.extraModulePackages = with config.boot.kernelPackages; [
  ];

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
  # https://wiki.archlinux.org/index.php/Solid_state_drive
  # https://wiki.debian.org/SSDOptimization
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  # Networking ===========================================

  # Define your hostname.
  networking.hostName = "yantra";

  # Nameservers to use
  networking.nameservers = [ "1.1.1.1" "1.0.0.1" ];

  # Use networkmanger (instead of wpa_supplicant)
  networking.networkmanager.enable = true;

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
    allowBroken = false;

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
    BROWSER = "firefox";
    _JAVA_AWT_WM_NONREPARENTING = "1"; # Java + Xmonad
    PATH = "$HOME/.local/bin:$PATH";
  };

  # List packages installed in system profile.
  environment.systemPackages = import ./packages.nix { inherit pkgs; } ++ import ./wayland-packages.nix { inherit pkgs; };

  # Fonts ================================================

  fonts.fonts = with pkgs; [
    ubuntu_font_family
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    noto-fonts-extra
    lohit-fonts.devanagari
    lohit-fonts.nepali
  ];

  # Programs =============================================

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.slock.enable = true;
  programs.sysdig.enable = true;

  # Set default editor
  programs.vim.defaultEditor = true;

  # Fish
  programs.fish.enable = true;

  # Sway
  programs.sway = {
    enable = true;
    extraSessionCommands = ''
      export SDL_VIDEODRIVER=wayland
      export QT_QPA_PLATFORM=wayland
      export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
      export EDITOR=vim
      export BROWSER=firefox
      export _JAVA_AWT_WM_NONREPARENTING=1
      export PATH="$HOME/.local/bin:$PATH"
      export XKB_DEFAULT_LAYOUT=us,us,se
      export XKB_DEFAULT_VARIANT=dvorak,,
      export XKB_DEFAULT_OPTIONS=grp:shifts_toggle,ctrl:nocaps
    '';
  };

  # Security =============================================

  security.sudo.enable = true;


  # Virtualisation =======================================

  virtualisation.docker.enable = true;
  virtualisation.docker.enableOnBoot = true;

  virtualisation.virtualbox.host.enable = false;

  # Services =============================================

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable acpi daemon
  services.acpid.enable = true;

  # Upower daemon - Culprit
  services.upower.enable = true;

  # Avahi daemon
  services.avahi.enable = true;
  services.avahi.nssmdns = true;

  # Dbus service
  services.dbus.enable = true;

  # Vnstat
  services.vnstat.enable = true;

  # Fwupd
  services.fwupd.enable = true;

  # Usbmuxd (Data to iOS)
  services.usbmuxd.enable = true;

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
  services.compton = {
    enable = true;
    backend = "xrender";
    activeOpacity = "1.0";
    inactiveOpacity = "0.8";
  };

  # Redshift
  services.redshift = {
    enable = true;
    latitude = "57.5";
    longitude = "12";
    temperature.day = 5500;
    temperature.night = 3700;
  };

  # Cadvisor
  services.cadvisor = {
    enable = true;
    port = 5050;
  };

  # Xserver
  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.autorun = true;

  services.xserver.layout = "us,us,se";
  services.xserver.xkbVariant = "dvorak,,";
  services.xserver.xkbOptions = "grp:shifts_toggle,ctrl:nocaps";

  services.xserver.autoRepeatDelay = 200;
  services.xserver.autoRepeatInterval = 60;

  # Pick an intel driver for this machine
  services.xserver.videoDrivers = [ "intel" "vesa" ];

  # Enable touchpad support.
  services.xserver.libinput.enable = true;
  services.xserver.libinput.clickMethod = "clickfinger";

  # Select a display manager (login manager)
  services.xserver.displayManager.lightdm.enable = true;

  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.bash}/bin/bash $HOME/.local/bin/random-wallpaper
  '';

  # Select a desktop manager
  services.xserver.desktopManager.default = "none";

  # Select a window manager
  services.xserver.windowManager.default = "xmonad";
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;

  # Users ================================================

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.suvash = {
    isNormalUser = true;
    createHome = true;
    extraGroups = ["wheel" "networkmanager" "docker" "vboxusers" "sway"];
    shell = "/run/current-system/sw/bin/fish";
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09";

}
