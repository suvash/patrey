{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    outputs.darwinModules.skhd

    ./settings.nix
    ./homebrew.nix
  ];

  # ENVIRONMENT =====================================================================

  # System level packages
  environment.systemPackages = with pkgs; [];

  # Shells
  environment.shells = [pkgs.fish];

  # Env vars
  environment.variables = {
    LANG = "en_US.UTF-8";
    PATREY_PATH = "$HOME/${config.settings.patreydir}";
  };

  # Create /etc/bashrc, /etc/zshrc and /etc/fish/... that loads the nix-darwin environment.
  programs.bash.enable = true;
  programs.zsh.enable = true;
  programs.fish.enable = true;

  # Needed to address bug where $PATH is not properly set for fish:
  # https://github.com/LnL7/nix-darwin/issues/122#issuecomment-1659465635
  programs.fish.loginShellInit = let
    # This naive quoting is good enough in this case. There shouldn't be any
    # double quotes in the input string, and it needs to be double quoted in case
    # it contains a space (which is unlikely!)
    dquote = str: "\"" + str + "\"";

    makeBinPathList = map (path: path + "/bin");
  in ''
    fish_add_path --move --prepend --path ${lib.concatMapStringsSep " " dquote (makeBinPathList config.environment.profiles)}
    set fish_user_paths $fish_user_paths
  '';

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

  system.activationScripts.setFishAsShell.text = ''
    dscl . -create /Users/${config.settings.username} UserShell /run/current-system/sw/bin/fish
  '';

  # NIX =============================================================================

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  # Auto upgrade nix package and the daemon service.
  nix.package = pkgs.nix;
  services.nix-daemon.enable = true;

  nix.settings = {
    # for using flakes
    experimental-features = "nix-command flakes";
    # for adding subtituters and their keys
    trusted-users = ["${config.settings.username}"];
  };

  # Set Git commit hash for darwin-version.
  # system.configurationRevision = self.rev or self.dirtyRev or null;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 5;

  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "x86_64-darwin";
}
