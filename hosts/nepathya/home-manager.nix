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

    ./settings.nix
    ./home-packages.nix

    # ../../modules/home-manager/neovim.nix
    # ../../modules/home-manager/tmux.nix
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
    };
  };

  nix.package = pkgs.nix;

  home.stateVersion = "24.11"; # DO NOT CHANGE THIS!!!

  home.username = "${config.settings.username}";
  home.homeDirectory = "/Users/${config.settings.username}";

  home.sessionVariables = {
    EDITOR = "${config.settings.editor}";
  };

  home.file = {
    # Needed for smart card to work
    ".gnupg/scdaemon.conf".text = "disable-ccid";

    # Hushlogin
    ".hushlogin".text = "For quieter logins. See `man login`.";
  };

  # Impure link
  xdg.configFile = {
    "karabiner/karabiner.json" = {
      source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/${config.settings.patreydir}/modules/impure/${config.settings.hostname}/karabiner.json";
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # man and info
  programs.man.enable = true;
  programs.info.enable = true;

  # Programs
  programs.autojump.enable = true;
  programs.bat.enable = true;

  programs.btop.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.fzf.enable = true;

  programs.htop.enable = true; # configure

  programs.tealdeer = {
    enable = true;
    settings = {
      display.compact = true;
      updates.auto_update = true;
    };
  };
}
