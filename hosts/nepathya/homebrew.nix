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
      # extraFlags = [ "--verbose" ];
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
      "Slack" = 803453959;
      "Tailscale" = 1475387142;
      "WhatsApp" = 310633997;
      "WireGuard" = 1451685025;
      "iNet Network Scanner" = 403304796;
    };

    casks = [
      "amethyst"
      "anki"
      "brave-browser"
      "calibre"
      "chrysalis"
      "claude"
      "cursor"
      "discord"
      "docker-desktop"
      "emacs-app"
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
      "knockknock"
      "latest"
      "linear-linear"
      "macwhisper"
      "microsoft-teams"
      "netiquette"
      "obsidian"
      "omnidisksweeper"
      "orion"
      "oversight"
      "qlcolorcode"
      "qlmarkdown"
      "qlstephen"
      "qlvideo"
      "raycast"
      "reikey"
      "repo-prompt"
      "sf-symbols"
      "shottr"
      "signal"
      "stats"
      "ungoogled-chromium"
      "utm"
      "vlc"
      "wacom-tablet"
      "windsurf"
      "yubico-authenticator"
      "zed"
      "zen"
      "zoom"
      "zotero"
      "zulip"
      { name = "apparency"; args = {require_sha = false;}; }
      { name = "lm-studio"; args = {appdir = "/Applications";}; }
      { name = "lulu"; args = {appdir = "/Applications";}; }
      { name = "onyx"; args = {require_sha = false;}; }
      { name = "quicklook-json"; args = {require_sha = false;}; }
      { name = "spotify"; args = {require_sha = false;}; }
      { name = "suspicious-package"; args = {require_sha = false;}; }
      { name = "vuescan"; args = {require_sha = false;}; }
      # Rosetta 2 needed
      # "disk-inventory-x" # using omnidisksweeper instead
      # "send-to-kindle" # using calibre instead
    ];
  };
}
