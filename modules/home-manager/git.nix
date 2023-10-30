{pkgs, ...}: {
  programs.git = {
    enable = true;
    userEmail = "suvash@gmail.com";
    userName = "Suvash Thapaliya";
    signing = {
      key = "5E73D2B2";
      signByDefault = false;
    };
    ignores = [
      "DS_Store"
      "*.swp"
    ];
    lfs.enable = true;
    diff-so-fancy.enable = true;
  };

  programs.gitui.enable = true;
}
