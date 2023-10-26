{pkgs, ...}: {
  environment.systemPackages = with pkgs.master; [
    tig
    gitAndTools.diff-so-fancy
    delta
  ];

  programs.git = {
    enable = true;
    package = pkgs.master.git;
    lfs = {
      enable = true;
      package = pkgs.master.git-lfs;
    };
  };
}
