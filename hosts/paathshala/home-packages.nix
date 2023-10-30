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

    # Hardware
    lshw
    hwinfo
    hardinfo
    lm_sensors
    lsof

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

    # linters
    yamllint
    ispell
    shellcheck

    # browser
    brave

    # communication
    slack
    discord
    zoom-us

    # bling
    cmatrix
    master.hollywood
  ];
}