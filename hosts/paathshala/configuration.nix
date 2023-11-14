{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    inputs.nixos-hardware.nixosModules.dell-xps-13-9360
    inputs.nixos-hardware.nixosModules.common-gpu-intel

    outputs.nixosModules.avahi
    outputs.nixosModules.pipewire
    outputs.nixosModules.yubikey

    ./settings.nix

    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  nixpkgs = {
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
      outputs.overlays.master-packages
      outputs.overlays.sha-3be4a51-packages

      # You can also add overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
    };
  };

  nix = {
    # This will add each flake input as a registry
    # To make nix3 commands consistent with your flake
    registry = lib.mapAttrs (_: value: {flake = value;}) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath =
      lib.mapAttrsToList (key: value: "${key}=${value.to.path}")
      config.nix.registry;

    # Automatic GC
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 8w";
    };

    settings = {
      # Enable flakes and new 'nix' command
      experimental-features = "nix-command flakes";
      # Deduplicate and optimize nix store
      auto-optimise-store = true;
      # Trusted users
      trusted-users = ["${config.settings.username}"];
    };
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 10;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.tmp = {
    useTmpfs = true;
    cleanOnBoot = true;
  };

  networking.hostName = "${config.settings.hostname}";
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable =
    true; # Easiest to use and most distros use this by default.

  # Use specific nameservers
  # Google NS
  # networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
  # Cloudflare NS
  networking.nameservers = ["1.1.1.1" "1.0.0.1"];

  # Open ports in the firewall.
  networking.firewall = {
    enable = true;
    checkReversePath = "strict";
    allowedUDPPorts = [7531];
    allowedTCPPorts = [7531];
  };

  # Set your time zone.
  time.timeZone = "${config.settings.timezone_ktm}";

  # Set hardware clock to local time
  time.hardwareClockInLocalTime = true;

  # Use chrony for ntp sync
  services.chrony.enable = true;

  # Power management
  powerManagement = {
    enable = true;
    powertop = {enable = true;};
  };

  # ACPILight
  hardware.acpilight.enable = true;

  # Bluetooth
  hardware.bluetooth.enable = true;

  # Firmware update service
  services.fwupd.enable = true;

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_GB.UTF-8";
    supportedLocales = ["all"];
  };

  console = {
    font = "latarcyrheb-sun32";
    useXkbConfig = true;
  };

  # Env vars
  environment.variables = {
    EDITOR = "${config.settings.edtr}";
    PATREY_PATH = "$HOME/patrey";
    DOCKER_HOST = "unix://$XDG_RUNTIME_DIR/docker.sock";
  };

  # Login/Lock screen image
  environment.etc."wallpapers/login.png" = {
    mode = "0555";
    source = ../../wallpapers/unlock/pawel-czerwinski-fPN1w7bIuNU-unsplash.png;
  };

  environment.etc."wallpapers/lock.png" = {
    mode = "0555";
    source = ../../wallpapers/unlock/pawel-czerwinski-zd50NyMmNVg-unsplash.png;
  };

  # Fonts
  fonts = {
    enableDefaultFonts = true;
    fontconfig = {enable = true;};
    fonts = with pkgs.unstable; [
      ubuntu_font_family
      roboto
      roboto-mono
      roboto-slab
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
      noto-fonts-extra
      lohit-fonts.devanagari
      lohit-fonts.nepali
      nerdfonts
      emacs-all-the-icons-fonts
    ];
  };

  # Virtualisation
  virtualisation.docker = {
    rootless.enable = true;
    autoPrune.enable = true;
    enableOnBoot.enable = true;
  };

  # Enable the X Server and autorun
  services.xserver.enable = true;
  services.xserver.autorun = true;

  # Configure keymap for X11
  services.xserver.layout = "us,us";
  services.xserver.xkbVariant = "dvorak,";
  services.xserver.xkbOptions = "grp:shifts_toggle,ctrl:nocaps";
  services.xserver.autoRepeatDelay = 200;
  services.xserver.autoRepeatInterval = 60;

  # Select a desktop manager - no window management
  services.xserver.desktopManager = {
    xterm.enable = false;
    xfce = {
      enable = true;
      enableScreensaver = false;
      noDesktop = true;
      enableXfwm = false;
    };
  };

  programs.thunar.enable = true;

  # Select a display/login manager
  services.xserver.displayManager = {
    defaultSession = "xfce+i3";
    sessionCommands = ''
      ${pkgs.xorg.xrdb}/bin/xrdb -merge <<EOF
      # 1928x180 on 13.3" -> 166
      # https://dpi.lv/#1920%C3%971080@13.3%E2%80%B3
      Xft.dpi: 166
      EOF
    '';
    lightdm = {
      enable = true;
      # https://github.com/NixOS/nixpkgs/issues/108289#issuecomment-758263467
      extraSeatDefaults = ''
        user-session = xfce+i3
      '';
      greeters.mini = {
        enable = true;
        user = "${config.settings.username}";
        # https://github.com/prikhi/lightdm-mini-greeter/blob/master/data/lightdm-mini-greeter.conf
        extraConfig = ''
          [greeter]
          show-password-label = false
          show-input-cursor = false
          password-alignment = left
          [greeter-theme]
          background-image = "/etc/wallpapers/login.png"
        '';
      };
    };
  };

  # Select a window manager
  services.xserver.windowManager = {
    i3 = {
      enable = true;
      package = pkgs.i3-gaps;
    };
  };

  # Touchpad
  services.xserver.libinput = {
    enable = true;
    touchpad = {
      tapping = true;
      disableWhileTyping = true;
      scrollMethod = "twofinger";
      naturalScrolling = false;
      middleEmulation = true;
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${config.settings.username} = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager" "video" "audio"];
    shell = pkgs.fish;
    openssh = {
      authorizedKeys = {
        keyFiles = [(../../keys + "/${config.settings.username}.ssh.key")];
      };
    };
  };

  programs.fish.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [vim wget git];

  systemd.services.console-blank = {
    enable = true;
    description = "Blank screen";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.util-linux}/bin/setterm -blank 1";
      TTYPath = "/dev/console";
      StandardOutput = "tty";
    };
    wantedBy = ["multi-user.target"];
    environment = {TERM = "linux";};
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # Mosh
  programs.mosh.enable = true;

  security.sudo = {
    enable = true;
    wheelNeedsPassword = true;
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable the Tailscale daemon
  services.tailscale.enable = true;

  # Bluetooth
  services.blueman.enable = true;

  # Logrotate
  services.logrotate.enable = true;

  # Vnstat
  services.vnstat.enable = true;

  # Fstrim
  services.fstrim.enable = true;

  # Enable ACPI daemon
  services.acpid.enable = true;

  # Enable UPower service
  services.upower.enable = true;

  # Enable TLP daemon
  services.tlp.enable = true;

  # Usbmuxd (Data to/from iOS)
  services.usbmuxd.enable = true;

  # udisks2 service
  services.udisks2.enable = true;

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
