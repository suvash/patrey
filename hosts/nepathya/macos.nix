{
  config,
  ...
}: {
  system.defaults = {
    dock = {
      appswitcher-all-displays = true;
      autohide = true;
      autohide-delay = 0.0;
      autohide-time-modifier = 0.0;
      dashboard-in-overlay = false;
      enable-spring-load-actions-on-all-items = true;
      expose-animation-duration = 0.2;
      expose-group-apps = false;
      largesize = 36;
      launchanim = true;
      magnification = false;
      mineffect = "scale";
      minimize-to-application = true;
      mouse-over-hilite-stack = false;
      mru-spaces = false;
      orientation = "bottom";
      persistent-apps = [];
      persistent-others = [];
      scroll-to-open = false;
      show-process-indicators = true;
      show-recents = false;
      showhidden = true;
      slow-motion-allowed = false;
      static-only = true;
      tilesize = 24;
      wvous-bl-corner = 11; # Launchpad
      wvous-br-corner = 5; # Start Screen Saver
      wvous-tl-corner = 4; # Desktop
      wvous-tr-corner = 12; # Notification Center
    };

    ".GlobalPreferences" = {
      "com.apple.sound.beep.sound" = "/System/Library/Sounds/Funk.aiff";
    };

    ActivityMonitor = {
      IconType = 6; # CPU Usage
      OpenMainWindow = true;
      ShowCategory = 100; # All Processes
      SortColumn = "CPUUsage";
      SortDirection = 0; # Descending
    };

    LaunchServices.LSQuarantine = true;

    NSGlobalDomain = {
      AppleEnableSwipeNavigateWithScrolls = true;
      AppleEnableMouseSwipeNavigateWithScrolls = true;
      AppleInterfaceStyleSwitchesAutomatically = true;
      AppleKeyboardUIMode = 3;
      ApplePressAndHoldEnabled = false;
      AppleScrollerPagingBehavior = true;
      AppleShowAllExtensions = true;
      AppleShowAllFiles = true;
      AppleShowScrollBars = "Always";
      AppleSpacesSwitchOnActivate = true;
      AppleTemperatureUnit = "Celsius";
      AppleMetricUnits = 1;
      AppleMeasurementUnits = "Centimeters";
      AppleWindowTabbingMode = "fullscreen";
      InitialKeyRepeat = 5;
      KeyRepeat = 1;
      NSAutomaticDashSubstitutionEnabled = false;
      NSAutomaticInlinePredictionEnabled = false;
      NSAutomaticPeriodSubstitutionEnabled = false;
      NSAutomaticQuoteSubstitutionEnabled = false;
      NSAutomaticSpellingCorrectionEnabled = false;
      NSAutomaticWindowAnimationsEnabled = false;
      NSDisableAutomaticTermination = true;
      NSDocumentSaveNewDocumentsToCloud = true;
      NSNavPanelExpandedStateForSaveMode = true;
      NSNavPanelExpandedStateForSaveMode2 = true;
      NSScrollAnimationEnabled = true;
      NSTableViewDefaultSizeMode = 2;
      NSTextShowsControlCharacters = true;
      NSUseAnimatedFocusRing = false;
      NSWindowResizeTime = 0.001;
      PMPrintingExpandedStateForPrint = true;
      PMPrintingExpandedStateForPrint2 = true;
      _HIHideMenuBar = false;
      "com.apple.keyboard.fnState" = false;
      "com.apple.mouse.tapBehavior" = 1;
      "com.apple.sound.beep.feedback" = 0;
      "com.apple.sound.beep.volume" = 0.7788;
      "com.apple.springing.delay" = 0.0;
      "com.apple.springing.enabled" = true;
      "com.apple.swipescrolldirection" = true;
      "com.apple.trackpad.enableSecondaryClick" = true;
      "com.apple.trackpad.forceClick" = true;
      "com.apple.trackpad.scaling" = 1.5;
    };

    WindowManager = {
      GloballyEnabled = false;
      EnableStandardClickToShowDesktop = false;
      EnableTiledWindowMargins = true;
      EnableTilingByEdgeDrag = true;
      EnableTopTilingByEdgeDrag = true;
      StandardHideDesktopIcons = true;
      StandardHideWidgets = true;
    };

    alf = {
      globalstate = 1;
      loggingenabled = 1;
      stealthenabled = 1;
      allowsignedenabled = 1;
      allowdownloadsignedenabled = 1;
    };

    finder = {
      AppleShowAllFiles = true;
      CreateDesktop = false;
      FXDefaultSearchScope = "SCcf";
      FXEnableExtensionChangeWarning = false;
      FXPreferredViewStyle = "clmv";
      FXRemoveOldTrashItems = false;
      NewWindowTarget = "Other";
      NewWindowTargetPath = "file:///Users/${config.settings.username}/";
      QuitMenuItem = true;
      ShowHardDrivesOnDesktop = false;
      ShowExternalHardDrivesOnDesktop = true;
      ShowMountedServersOnDesktop = true;
      ShowRemovableMediaOnDesktop = true;
      ShowPathbar = true;
      ShowStatusBar = true;
      _FXShowPosixPathInTitle = true;
      _FXSortFoldersFirst = true;
      _FXSortFoldersFirstOnDesktop = true;
    };

    hitoolbox.AppleFnUsageType = "Start Dictation";

    loginwindow = {
      DisableConsoleAccess = true;
      GuestEnabled = false;
      LoginwindowText = "${config.settings.computername}";
      PowerOffDisabledWhileLoggedIn = false;
      RestartDisabled = false;
      RestartDisabledWhileLoggedIn = false;
      SHOWFULLNAME = false;
      ShutDownDisabled = false;
      ShutDownDisabledWhileLoggedIn = false;
      SleepDisabled = false;
      autoLoginUser = null;
    };

    menuExtraClock.IsAnalog = true;

  };
}
