{pkgs, ...}: {
  home.packages = with pkgs; [
    cmake
    gnumake

    # Pulseaudio
    paprefs
    ncpamixer
    pamixer
    pamix

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

    # creds
    authy

    # browser
    brave

    # xfce Utilities
    # for keyboard volume buttons
    xfce.xfce4-volumed-pulse

    # communication
    slack
    discord
    zoom-us

    # editors
    obsidian

    # bling
    cmatrix
    neofetch
    master.hollywood
  ];
}
