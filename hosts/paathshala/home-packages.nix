{pkgs, ...}: {
  home.packages = with pkgs; [
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
    sha-3be4a51.tree

    # X server
    brightnessctl
    arandr
    libnotify
    xclip
    xsel

    # Hardware
    lshw
    hwinfo
    hardinfo
    lm_sensors
    lsof
    lsb-release
    dmidecode

    # webcam
    v4l-utils
    gnome.cheese

    # monitoring
    iotop
    iftop
    unstable.dfc
    duf
    unstable.du-dust
    dstat
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

    # console
    parallel

    # dns
    dnsutils
    whois

    # linters / formatters
    yamllint
    ispell
    shellcheck
    shfmt
    nixfmt

    # infrastructure
    flyctl

    # browser
    brave

    # xfce Utilities
    # for keyboard volume buttons
    xfce.xfce4-volumed-pulse

    # communication
    slack
    discord
    zoom-us
    webex

    # editors
    obsidian

    # bling
    cmatrix
    neofetch
    master.hollywood
  ];
}
