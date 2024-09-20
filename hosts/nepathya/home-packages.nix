{
  pkgs,
  config,
  ...
}: {
  home.packages = with pkgs; [
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
    fzf
    which
    less
    rlwrap
    ripgrep
    tree
    ranger

    # Watchers followers etc
    watch
    direnv

    # Compression
    gzip
    unzip
    pigz

    # System tools
    htop
    btop
    glances
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

    # Development tools
    tokei
    pgcli

    # Documentation tools
    tldr

    # Terminals / shells
    fasd
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

    # Scripts
    (pkgs.writeShellScriptBin "my-hello" ''
      echo "Hello, ${config.home.username}!"
    '')
  ];
}
