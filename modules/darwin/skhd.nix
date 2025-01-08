{
  config,
  pkgs,
  ...
}: {
  services.skhd = {
    enable = true;
    skhdConfig = ''
      cmd + shift - r : ${pkgs.skhd}/bin/skhd --reload
      cmd + shift - return : open -n /Applications/kitty.app

      ctrl + cmd - e : open -n /Applications/Emacs.app
      ctrl + cmd - l : open /Applications/Slack.app
      ctrl + cmd - m : open /System/Applications/Music.app
      ctrl + cmd - f : open -n /Applications/Firefox.app
      ctrl + cmd - s : open -n /Applications/Safari.app
      ctrl + cmd - b : /Applications/Brave\ Browser.app/Contents/MacOS/Brave\ Browser --incognito
      ctrl + cmd - c : /Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome --incognito
    '';
  };
}
