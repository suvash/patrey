{ pkgs, ... }: {
  security.rtkit.enable = true;

  environment.systemPackages = with pkgs; [ playerctl ];

  programs.dconf.enable = true;

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    #jack.enable = true;
    wireplumber.enable = true;
  };
}
