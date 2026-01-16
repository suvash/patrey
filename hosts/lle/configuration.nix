# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  outputs,
  config,
  pkgs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    # common modules
    outputs.nixosModules.avahi

    # secrets & cloudflared
    ./sops.nix
    ./cloudflared.nix

    # media serving
    ./plex.nix
    ./sabnzbd.nix
    ./prowlarr.nix
    ./radarr.nix
    ./sonarr.nix

    # home automation
    ./home-assistant.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 10;
  boot.loader.efi.canTouchEfiVariables = true;

  # Turn off bluetooth
  hardware.bluetooth.enable = false;
  # Blacklist broken bluetooth kernel modules
  boot.blacklistedKernelModules = ["bluetooth" "btusb" "btrtl" "btbcm" "btmtk" "btintel"];

  boot.tmp = {
    useTmpfs = true;
    cleanOnBoot = true;
  };

  nix = {
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
      trusted-users = ["@wheel"];
    };
  };

  # Nixos
  nixpkgs.config.allowUnfree = true;

  # Define your hostname.
  networking.hostName = "lle";

  # Configure network connections interactively with nmcli or nmtui.
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Stockholm";

  # Set hardware clock to local time
  time.hardwareClockInLocalTime = false;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Enable the X11 windowing system.
  # services.xserver.enable = true;

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # services.pulseaudio.enable = true;
  # OR
  # services.pipewire = {
  #   enable = true;
  #   pulse.enable = true;
  # };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.libinput.enable = true;

  # User accounts
  users.users.d = {
    isNormalUser = true;
    extraGroups = [
      "${config.users.groups.wheel.name}"
      "${config.users.groups.media.name}"
      "${config.users.groups.keys.name}"
      "${config.users.groups.dialout.name}"
    ];
    packages = with pkgs; [
      cmatrix
    ];
    openssh = {
      authorizedKeys = {
        keyFiles = [../../keys/suvash.ssh.keys];
      };
    };
  };

  # Media folder permissions
  users.groups.media = {
    gid = 1001;
  };

  users.users.media = {
    isSystemUser = true;
    group = "${config.users.groups.media.name}";
    uid = 1001;
    description = "Media files owner";
  };

  systemd.tmpfiles.rules = [
    "d /media 2770 ${config.users.users.media.name} ${config.users.groups.media.name} -"
  ];

  # programs.firefox.enable = true;

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  environment.systemPackages = with pkgs; [
    vim
    wget
    btop
    dust
    dfc
    git
    lsof
    psmisc
    tree
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Firmware update service
  services.fwupd.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Tailscale
  services.tailscale.enable = true;

  # Vnstat
  services.vnstat.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "25.11"; # Did you read the comment?
}
