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
      "Hand Mirror" = 1502839586;
      "Mactracker" = 430255202;
      "NetSpot" = 514951692;
      "Save to Pocket" = 1477385213;
      "Slack" = 803453959;
      "Tailscale" = 1475387142;
      "Vimari" = 1480933944;
      "iNet Network Scanner" = 403304796;
      "Todoist" = 585829637;
      "Blinks" = 998804308;
    };

    casks = [
      "amethyst"
      "anki"
      "authy"
      "arc"
      "brave-browser"
      "blackhole-16ch"
      "chrysalis"
      "coconutbattery"
      "cursor"
      "dbeaver-community"
      "discord"
      "disk-inventory-x"
      "docker"
      "firefox"
      "figma"
      "google-chrome"
      "iterm2"
      "thunderbird"
      "karabiner-elements"
      "keepingyouawake"
      "kensingtonworks"
      "keka"
      "kekaexternalhelper"
      "kindle"
      "kitty"
      "libreoffice"
      "linear-linear"
      "notion"
      "obsidian"
      "postman"
      "raycast"
      "signal"
      "spotify"
      "send-to-kindle"
      "vlc"
      "visual-studio-code"
      "webex"
      "wacom-tablet"
      "whatsapp"
      "zotero"
      "zoom"
      "zulip"
      "zed"
    ];
  };
}
