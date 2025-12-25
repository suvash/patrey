{...}: {
  programs.git = {
    enable = true;
    settings = {
      user.name = "Suvash Thapaliya";
      user.email = "suvash@gmail.com";
      init = {
        defaultBranch = "main";
      };
    };
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
    lfs.enable = true;
  };

  programs.diff-so-fancy = {
    enable = true;
    enableGitIntegration = true;
  };
  programs.gitui.enable = true;
  programs.lazygit.enable = true;
}
