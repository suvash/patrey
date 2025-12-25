{
  pkgs,
  inputs,
  ...
}: {
  home.packages = with pkgs;
    [
      cmake
      gnumake

      # Pulseaudio
      paprefs
      ncpamixer
      pamixer
      pamix
      pavucontrol

      # Entertainment
      spotify
      vlc

      # File tools
      fd
      unstable.ripgrep
      master.silver-searcher
      pigz
      unzip
      tree

      # X server
      brightnessctl
      arandr
      libnotify
      xclip
      xsel

      # Hardware
      lshw
      hwinfo
      lm_sensors
      lsof
      lsb-release
      dmidecode

      # webcam
      v4l-utils
      cheese

      # monitoring
      iotop
      iftop
      unstable.dfc
      duf
      unstable.dust
      powertop
      ncdu
      baobab

      # network
      iw
      ethtool
      traceroute
      gping
      nmap
      master.cfspeedtest
      speedtest-cli
      magic-wormhole
      wakeonlan

      # console
      parallel

      # dns
      dnsutils
      whois

      # linters / formatters
      yamllint
      shellcheck
      ispell

      # infrastructure
      flyctl

      # browser
      brave

      # editors
      unstable.code-cursor
      unstable.zed-editor

      # xfce Utilities
      # for keyboard volume buttons
      xfce.xfce4-volumed-pulse

      # communication
      thunderbird
      slack
      zulip
      discord
      zoom-us
      webex
      libreoffice

      # editors
      obsidian
      figma-linux

      # benchmark
      geekbench

      # documents
      calibre

      # bling
      cmatrix
      neofetch
      master.hollywood
    ];
}
