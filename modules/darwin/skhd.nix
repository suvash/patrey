{
  config,
  pkgs,
  ...
}: {
  services.skhd = {
    enable = true;
    skhdConfig = ''
      cmd + shift - r : ${pkgs.skhd}/bin/skhd --reload
      cmd + shift - return : open -n ~/Applications/kitty.app

      ctrl + cmd - s : open /Applications/Safari.app

      ctrl + cmd - f : open ~/Applications/Firefox.app
      ctrl + cmd - o :  open ~/Applications/Orion.app
      ctrl + cmd - b : ~/Applications/Brave\ Browser.app/Contents/MacOS/Brave\ Browser --incognito
    '';
  };
}
