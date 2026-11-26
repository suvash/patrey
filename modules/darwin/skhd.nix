{
  config,
  pkgs,
  ...
}: {
  services.skhd = {
    enable = false;
    skhdConfig = ''
      cmd + shift - r : ${pkgs.skhd}/bin/skhd --reload
    '';
  };
}
