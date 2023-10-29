{
  inputs,
  pkgs,
  ...
}: {
  programs.fish = {
    enable = true;
    interactiveShellInit = ''
      set BASE16_SHELL_PATH "${inputs.base16-shell}"
      source "$BASE16_SHELL_PATH/profile_helper.fish"
    '';
    functions = {
      getignore = "curl -sL https://www.gitignore.io/api/$argv";
      set_base16_theme = {
        description = "set a base16 based shell theme";
        body = ''
          set theme "base16-$argv"
          eval $theme
        '';
      };
      toggle_shell_theme = {
        description = "toggle between dark & light theme";
        body = ''
          set theme_light "one-light"
          set theme_dark "onedark"
          set target ""
          switch $BASE16_THEME
          case $theme_light
            set target $theme_dark
          case $theme_dark
            set target $theme_light
          case "*"
            set target $theme_dark
          end
          set subject "Toggling light/dark SHELL theme"
          set body "Switching to $target theme"
          ${pkgs.libnotify}/bin/notify-send --urgency=NORMAL $subject $body
          set_base16_theme $target
        '';
      };
    };
    shellAbbrs = {
      "..." = "../..";
      "...." = "../../..";
      "....." = "../../../..";
      "tst" = "toggle_shell_theme";
    };
  };
}
