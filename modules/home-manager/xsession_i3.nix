{
  pkgs,
  lib,
  ...
}: {
  xsession.windowManager.i3 = {
    enable = true;
    config = rec {
      modifier = "Mod4";

      fonts = {
        size = 10.0;
      };

      bars = [
        {
          position = "bottom";
          statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs  ~/.config/i3status-rust/config-laptop.toml";
          fonts = {
            size = 10.0;
          };
        }
      ];

      window.commands = [
        {
          command = "floating enable, move position 15 ppt 15ppt, resize set 70 ppt 70 ppt, border pixel 1";
          criteria = {
            class = "__scratch_terminal";
          };
        }
      ];

      keybindings = lib.mkOptionDefault {
        "${modifier}+d" = "exec ${pkgs.rofi}/bin/rofi -show drun";

        "${modifier}+j" = "focus left";
        "${modifier}+k" = "focus down";
        "${modifier}+l" = "focus up";
        "${modifier}+semicolon" = "focus right";

        "${modifier}+Shift+j" = "move left";
        "${modifier}+Shift+k" = "move down";
        "${modifier}+Shift+l" = "move up";
        "${modifier}+Shift+semicolon" = "move right";
        "${modifier}+n" = "move workspace to output next";

        "${modifier}+ctrl+3" = "exec ${pkgs.flameshot}/bin/flameshot full --path ~/.screenshots";
        "${modifier}+ctrl+4" = "exec ${pkgs.flameshot}/bin/flameshot gui --path ~/.screenshots";

        "${modifier}+Shift+e" = "exec ${pkgs.xfce.xfce4-session}/bin/xfce4-session-logout";

        "${modifier}+ctrl+6" = "exec ${pkgs.playerctl}/bin/playerctl previous";
        "${modifier}+ctrl+8" = "exec ${pkgs.playerctl}/bin/playerctl play-pause";
        "${modifier}+ctrl+0" = "exec ${pkgs.playerctl}/bin/playerctl next";

        "${modifier}+ctrl+v" = "exec CM_LAUNCHER=rofi ${pkgs.clipmenu}/bin/clipmenu";
        "${modifier}+ctrl+w" = "exec ${pkgs.feh}/bin/feh --bg-tile --no-fehbg --randomize ~/.wallpapers";
        "${modifier}+ctrl+t" = "exec ${pkgs.fish}/bin/fish -c toggle_xfce_theme";
        "${modifier}+ctrl+l" = "exec ${pkgs.i3lock}/bin/i3lock --nofork --ignore-empty-password --show-failed-attempts --image /etc/wallpapers/lock.png";

        "${modifier}+ctrl+space" = "exec ${pkgs.kitty}/bin/kitty --class='__scratch_terminal'"; # check scratchpad

        "${modifier}+ctrl+Up" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +10%";
        "${modifier}+ctrl+Down" = "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -10%";

        "${modifier}+ctrl+e" = "exec emacs";
        "${modifier}+ctrl+f" = "exec firefox";
        "${modifier}+ctrl+b" = "exec brave --incognito";
        "${modifier}+ctrl+c" = "exec chromium --incognito";
        "${modifier}+ctrl+q" = "exec qutebrowser";
        "${modifier}+ctrl+s" = "exec spotify";
        "${modifier}+ctrl+x" = "exec systemctl --user restart xcape";
      };

      gaps = {
        inner = 2;
        smartGaps = true;
        smartBorders = "on";
      };
    };

    extraConfig = ''
    '';
  };
}
