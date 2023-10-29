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
      toggle_shell_theme = {
        description = "toggle between dark & light theme for shell";
        body = ''
          set theme_light "one-light"
          set theme_dark "onedark"
          switch $BASE16_THEME
          case $theme_light
            set target $theme_dark
          case $theme_dark
            set target $theme_light
          case "*"
            set target $theme_dark
          end
          set theme_target "base16-$target"
          set subject "Toggling light/dark SHELL theme"
          set body "Switching to $target theme"
          ${pkgs.libnotify}/bin/notify-send --urgency=NORMAL $subject $body
          eval $theme_target
        '';
      };
      get_xfce_theme = {
        description = "get current xfce theme";
        body = ''
          set command "xfconf-query -c xsettings -p /Net/ThemeName"
          eval $command
        '';
      };
      set_xfce_theme = {
        description = "set a xfce theme";
        body = ''
          set theme $argv
          set command "xfconf-query -c xsettings -p /Net/ThemeName -s $theme"
          eval $command
        '';
      };
      toggle_xfce_theme = {
        description = "toggle between dark & light theme for xfce";
        body = ''
          set theme_light "Adwaita"
          set theme_dark "Adwaita-dark"
          set theme_current (get_xfce_theme)
          set theme_target ""
          switch $theme_current
          case $theme_light
            set theme_target $theme_dark
          case $theme_dark
            set theme_target $theme_light
          case "*"
            set theme_target $theme_dark
          end
          set subject "Toggling light/dark XFCE theme"
          set body "Switching to $theme_target theme"
          ${pkgs.libnotify}/bin/notify-send --urgency=NORMAL $subject $body
          set_xfce_theme $theme_target
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
