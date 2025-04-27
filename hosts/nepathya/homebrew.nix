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

    caskArgs = {
      appdir = "~/Applications";
      require_sha = true;
    };

    brews = [
      {
        name = "emacs-plus@30";
        args = ["with-imagemagick" "with-modern-doom3-icon"];
      }
    ];

    masApps = {
      "Bitwarden" = 1352778147;
      # "Blinks" = 998804308;
      # "Hand Mirror" = 1502839586;
      # "Mactracker" = 430255202;
      # "NetSpot" = 514951692;
      # "Save to Pocket" = 1477385213;
      # "Slack" = 803453959;
      # "Tailscale" = 1475387142;
      # "WhatsApp" = 310633997;
      # "iNet Network Scanner" = 403304796;
    };

    casks = [
      "brave-browser"
      "ente-auth"
      "firefox"
      "karabiner-elements"
      "kitty"
      "orion"
      "zed"
    ];
  };
}
