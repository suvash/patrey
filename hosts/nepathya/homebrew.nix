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

    taps = [];

    caskArgs = {
      appdir = "~/Applications/Casks";
      require_sha = true;
    };

    brews = [];

    masApps = {
      "Amphetamine" = 937984704;
      "Bitwarden" = 1352778147;
      "Blinks" = 998804308;
      "GarageBand" = 682658836;
      "Hand Mirror" = 1502839586;
      "Keynote" = 409183694;
      "Numbers" = 409203825;
      "Pages" = 409201541;
      "iMovie" = 408981434;
      "Mactracker" = 430255202;
      "NetSpot" = 514951692;
      "Save to Pocket" = 1477385213;
      "Slack" = 803453959;
      "Tailscale" = 1475387142;
      "WhatsApp" = 310633997;
      "iNet Network Scanner" = 403304796;
    };

    casks = [
      "anki"
      "brave-browser"
      "calibre"
      "cursor"
      "discord"
      "disk-inventory-x"
      "docker"
      "emacs"
      "ente-auth"
      "figma"
      "firefox"
      "iina"
      "karabiner-elements"
      "keka"
      "kekaexternalhelper"
      "kitty"
      "linear-linear"
      "obsidian"
      "orion"
      "raycast"
      "signal"
      "vlc"
      "zed"
      "zoom"
      "zulip"
      # "chrysalis"
      # "coconutbattery"
      # "keepingyouawake"
      # "kensingtonworks"
      # "kindle"
      # "send-to-kindle"
      # "shottr"
      # "wacom-tablet"
      # "windsurf"
      # "zotero"
      { name = "google-chrome"; args = {require_sha = false;}; }
      { name = "spotify"; args = {require_sha = false;}; }
    ];
  };
}
