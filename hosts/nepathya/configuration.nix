{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    ./settings.nix
    ./homebrew.nix
  ];

  # ENVIRONMENT =====================================================================

  # System level packages
  environment.systemPackages = with pkgs; [cmatrix git];

  # Env vars
  environment.variables = {
    LANG = "en_US.UTF-8";
    PATREY_PATH = "$HOME/Developer/patrey";
    EDITOR = "${config.settings.editor}";
  };

  # Create /etc/bashrc, /etc/zshrc and /etc/fish/... that loads the nix-darwin environment.
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.fish.enable = true;

  # NETWORKING ======================================================================

  networking = {
    computerName = "${config.settings.computername}";
    hostName = "${config.settings.hostname}";
    localHostName = "${config.settings.hostname}";

    # cloudflare dns
    dns = ["1.1.1.1" "1.0.0.1" "2606:4700:4700::1111" "2606:4700:4700::1001"];

    # networksetup -listallnetworkservices
    knownNetworkServices = ["Wi-Fi" "iPhone USB"];
  };

  # PROGRAMS ========================================================================

  # GPG agent with SSH support
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # USERS ===========================================================================

  users.users.${config.settings.username} = {
    name = "${config.settings.username}";
    home = "/Users/${config.settings.username}";
  };

  # NIX =============================================================================

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  # Auto upgrade nix package and the daemon service.
  nix.package = pkgs.nix;
  services.nix-daemon.enable = true;

  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  # Set Git commit hash for darwin-version.
  # system.configurationRevision = self.rev or self.dirtyRev or null;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "x86_64-darwin";
}
