{
  config,
  pkgs,
  ...
}: {
  services.skhd = {
    enable = true;
    skhdConfig = ''
      cmd + shift - r : ${pkgs.skhd}/bin/skhd --reload
      cmd + shift - return : open -n ~/Applications/Casks/kitty.app

      ctrl + cmd - s : open /Applications/Safari.app

      ctrl + cmd - f : open ~/Applications/Casks/Firefox.app
      ctrl + cmd - o :  open ~/Applications/Casks/Orion.app
      ctrl + cmd - b : ~/Applications/Casks/Brave\ Browser.app/Contents/MacOS/Brave\ Browser --incognito
    '';
  };
}
