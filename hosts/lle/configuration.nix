# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  config,
  lib,
  pkgs,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 10;
  boot.loader.efi.canTouchEfiVariables = true;

  # Blacklist broken bluetooth kernel modules
  boot.blacklistedKernelModules = ["bluetooth" "btusb" "btrtl" "btbcm" "btmtk" "btintel"];

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

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.d = {
    isNormalUser = true;
    extraGroups = ["wheel"]; # Enable ‘sudo’ for the user.
    packages = with pkgs; [
      cmatrix
    ];
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN60SfuDBLVVS0llvLUK6z+OMiObjycudi9LTfnsOp5X"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDGnQCHl/oI8aceWyPBmlGUZ3fxXZl8kWU2rBAvv6mYmGDHzciJZDl+7IAZ00XuV8BcvFN0FjlSaQBULMoBZhDGwrB1Kuz921xF1LTt1trLbEo+4ZYCIOwTx7+CPuMAbzuNdmaWihijZb1JFko5kcjrsvNyAPzLPeGv0FF/hA/ZeISCFRjgV+lhxxnvjgYeeMawcbUdcMJoJHJVi/gcbLwMMrmC4MLZtpiQumsLPvoXLCiG7uTNdyrAXI2SpaaQzjSj4DzK5D+9C1bGSbwVpsbmoUDyvzWH3LpaeHlmA+il6KhFOjHHHYUHBR12eLGvYd3Huib6oWubhqMWBj7MlAHViFkJ612PWpZ4hflnTC9DidJ85W3hqxSo08D/+BT5mS/ROBxYpGsMxWePRnHru0Xm3ZpYRyyv1gS3A+A0sJdlnzZWZgWqkFSe+UEJsoixfQM+FptsN0DgIGOR0GbIUZwtIfpQ1RgiaMsVmSoKy8Qngz1bA+nHwrC7YYFWHx0anls9XdQAPtabGgicQuvDT0vwveF4iHeA+n/JOn+VCkcaf3zxD5WmPDQk3ZhK6/Zw/6BMC9p3NNEuBy/BsfWNMgsX2FRdhBgpBSy84ttMMGmt69v3Cc36oTRegUtpLi/nVvO6uI/zK40vRq24G4ukBlVvEYRYNhFz5a0Rg5Lc1eUfrQ=="
    ];
  };

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
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Avahi
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      enable = true;
      addresses = true;
      workstation = true;
    };
  };

  # Tailscale
  services.tailscale.enable = true;

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
