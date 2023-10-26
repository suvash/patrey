# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running `nixos-help`).
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
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

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
      trusted-users = ["suvash"];
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

  networking.hostName = "paathshala"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.

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

  # Per-interface useDHCP
  networking.useDHCP = false;
  networking.interfaces.wlp58s0.useDHCP = true;
  networking.interfaces.enp0s20f0u1u3.useDHCP = true;

  # Set your time zone.
  time.timeZone = "Asia/Katmandu";

  # Set hardware clock to local time
  time.hardwareClockInLocalTime = true;

  # Use chrony for ntp sync
  services.chrony.enable = true;

  # Power management
  powerManagement = {
    enable = true;
    powertop = {enable = true;};
  };

  # Firmware update service
  services.fwupd.enable = true;

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  console = {
    font = "latarcyrheb-sun32";
    keyMap = "us";
    #useXkbConfig = true; # use xkbOptions in tty.
  };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.suvash = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"]; # Enable ‘sudo’ for the user.
    # packages = with pkgs; [
    #   firefox
    #   tree
    # ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    [
      git
      vim
      wget
    ]
    ++ [pkgs.master.dfc pkgs.unstable.ripgrep pkgs.sha-3be4a51.tree];

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
    environment = {
      TERM = "linux";
    };
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

  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.05"; # Did you read the comment?
}
