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
      "chrysalis"
      "claude"
      "cursor"
      "discord"
      "docker"
      "eloston-chromium"
      "emacs"
      "ente-auth"
      "figma"
      "firefox"
      "ghostty"
      "hovrly"
      "iina"
      "jordanbaird-ice"
      "karabiner-elements"
      "keka"
      "kekaexternalhelper"
      "keycastr"
      "kitty"
      "latest"
      "linear-linear"
      "obsidian"
      "orion"
      "raycast"
      "sf-symbols"
      "shottr"
      "signal"
      "stats"
      "vlc"
      "wacom-tablet"
      "windsurf"
      "zed"
      "zoom"
      "zotero"
      "zulip"
      { name = "lm-studio"; args = {appdir = "/Applications";}; }
      { name = "onyx"; args = {require_sha = false;}; }
      { name = "spotify"; args = {require_sha = false;}; }
      { name = "vuescan"; args = {require_sha = false;}; }
      # Rosetta 2 needed
      # "disk-inventory-x"
      # "send-to-kindle"
    ];
  };
}
