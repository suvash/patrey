{
  outputs,
  config,
  pkgs,
  ...
}: {
  imports = [
    outputs.homeManagerModules.git
    outputs.homeManagerModules.fish

    ./settings.nix
    ./sway-home.nix
    ./home-packages.nix
  ];

  nixpkgs = {
    overlays = [
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
    };
  };

  home.stateVersion = "25.11"; # DO NOT CHANGE THIS!!!

  home.username = "${config.settings.username}";
  home.homeDirectory = "/home/${config.settings.username}";

  home.file.".screenshots/.keep".text = "";

  home.sessionVariables = {
  };

  programs.home-manager.enable = true;

  # Programs
  # programs.kitty = {
  #   enable = true;
  #   package = pkgs.unstable.kitty;
  # };
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.firefox = {
    enable = true;

    languagePacks = ["en-UK"];

    policies = {
      DisableTelemetry = true;
    };
  };
}
