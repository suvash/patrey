{...}: {
  programs.git = {
    enable = true;
    userEmail = "suvash@gmail.com";
    userName = "Suvash Thapaliya";
    signing = {
      key = "5E73D2B2";
      signByDefault = false;
    };
    ignores = [
      # Apple stuff
      ".DS_Store"

      # Editors
      "*.swp"

      # Secrets
      "*.env"
    ];
    extraConfig = {
      init = {
        defaultBranch = "main";
      };
    };
    lfs.enable = true;
    diff-so-fancy.enable = true;
  };

  programs.gitui.enable = true;
  programs.lazygit.enable = true;
}
