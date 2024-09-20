{
  inputs,
  outputs,
  config,
  pkgs,
  ...
}: {
  imports = [
    outputs.homeManagerModules.git
    outputs.homeManagerModules.fish
    outputs.homeManagerModules.starship

    ./settings.nix
    ./home-packages.nix

    ../../modules/home-manager/neovim.nix
    ../../modules/home-manager/tmux.nix
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.unstable-packages
    ];
    # Configure your nixpkgs instance
    config = {
      allowUnfree = true;
    };
  };

  home.stateVersion = "24.05"; # DO NOT CHANGE THIS!!!

  home.username = "${config.settings.username}";
  home.homeDirectory = "/Users/${config.settings.username}";

  home.sessionVariables = {
    EDITOR = "${config.settings.editor}";
  };

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # Needed for smart card to work
    # ".gnupg/scdaemon.conf".text = "disable-ccid";

    # Hushlogin
    ".hushlogin".text = "For quieter logins. See `man login`.";

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
