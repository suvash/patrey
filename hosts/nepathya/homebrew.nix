{...}: {
  homebrew = {
    enable = true;

    global = {
      brewfile = true;
      autoUpdate = false;
    };

    onActivation = {
      autoUpdate = false;
      upgrade = false;
      cleanup = "zap";
    };

    taps = [
      "d12frosted/emacs-plus"
    ];

    brews = [
      {
        name = "emacs-plus";
        args = ["with-modern-doom3-icon" "with-native-comp"];
      }
    ];

    masApps = {
      "Bitwarden" = 1352778147;
      "Blinks" = 998804308;
      "Hand Mirror" = 1502839586;
      "Mactracker" = 430255202;
      "NetSpot" = 514951692;
      "Save to Pocket" = 1477385213;
      "Slack" = 803453959;
      "Tailscale" = 1475387142;
      "Todoist" = 585829637;
      "Vimari" = 1480933944;
      "WhatsApp" = 310633997;
      "iNet Network Scanner" = 403304796;
    };

    casks = [
      "amethyst"
      "anki"
      "arc"
      "authy"
      "blackhole-16ch"
      "brave-browser"
      "calibre"
      "chrysalis"
      "coconutbattery"
      "cursor"
      "dbeaver-community"
      "discord"
      "disk-inventory-x"
      "docker"
      "figma"
      "firefox"
      "google-chrome"
      "iterm2"
      "karabiner-elements"
      "keepingyouawake"
      "keka"
      "kekaexternalhelper"
      "kensingtonworks"
      "kindle"
      "kitty"
      "libreoffice"
      "linear-linear"
      "notion"
      "obsidian"
      "postman"
      "raycast"
      "send-to-kindle"
      "signal"
      "spotify"
      "thunderbird"
      "visual-studio-code"
      "vlc"
      "wacom-tablet"
      "webex"
      "zed"
      "zoom"
      "zotero"
      "zulip"
    ];
  };
}
