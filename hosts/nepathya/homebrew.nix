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
        name = "emacs-plus@30";
        args = ["with-modern-doom3-icon"];
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
      "authy"
      "brave-browser"
      "calibre"
      "chrysalis"
      "coconutbattery"
      "cursor"
      "discord"
      "disk-inventory-x"
      "docker"
      "figma"
      "firefox"
      "google-chrome"
      "iina"
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
      "raycast"
      "send-to-kindle"
      "signal"
      "shottr"
      "spotify"
      "visual-studio-code"
      "vlc"
      "wacom-tablet"
      "windsurf"
      "zed"
      "zoom"
      "zotero"
      "zulip"
    ];
  };
}
