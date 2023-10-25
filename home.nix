{
  config,
  pkgs,
  ...
}: {
  programs.home-manager.enable = true;

  home.stateVersion = "23.05";

  home.username = "suvash";
  home.homeDirectory = "/home/suvash";

  home.packages = with pkgs; [
    cmatrix
    nixpkgs-fmt
  ];
}
