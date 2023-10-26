{pkgs, ...}: {

  programs.git = {
    enable = true;
    lfs.enable = true;
    diff-so-fancy.enable = true;
  };

  programs.gitui.enable = true;
}
