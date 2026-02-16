{
  pkgs,
  config,
  ...
}: {
  home.packages = with pkgs; [
    # Nix cache
    cachix

    # GNU bundles
    coreutils-full
    binutils
    findutils

    # GNU
    gawk
    gnused
    gnutar
    gnumake
    gnutls
    gnupg
    gnugrep

    # File & Directory tools
    fd
    which
    less
    rlwrap
    ripgrep
    tree
    ranger

    # Watchers followers etc
    watch

    # Compression
    gzip
    unzip
    pigz

    # System tools
    lsof
    pstree

    # Git family
    git
    tig

    # Disk tools
    dfc
    duf
    dust

    # Network tools
    curl
    wget
    whois
    ngrep
    gping
    nmap
    wakeonlan
    dogdns
    magic-wormhole
    trippy
    stuntman

    # Development tools
    tokei
    pgcli

    # hardware tools
    yubikey-manager

    # media tools
    imagemagick
    ffmpeg

    # Terminals / shells
    tmux
    zellij
    mosh

    # Format parsers
    jq
    yq

    # Linters & Checkers etc
    ispell
    yamllint
    shellcheck

    # Benchmark
    speedtest-cli

    # bling
    cmatrix
    neofetch
  ];
}
