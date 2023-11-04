{ pkgs, ... }: {
  environment.systemPackages = with pkgs; [
    paprefs
    ncpamixer
    pamixer
    pamix
    playerctl
  ];

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };
}
