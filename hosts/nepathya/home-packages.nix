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
    du-dust

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

    # Terminals / shells
    tmux
    zellij
    mosh

    # Format parsers
    jq
    yq
    xsv

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
