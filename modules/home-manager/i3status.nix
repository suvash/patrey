{pkgs, ...}: {
  programs.i3status-rust = {
    enable = true;
    bars = {
      laptop = {
        theme = "plain";
        icons = "awesome4";
        blocks = [
          {
            block = "focused_window";
            format = "$title.str(max_w:25) |";
          }

          {
            block = "backlight";
          }

          {
            block = "music";
          }

          {
            block = "cpu";
            interval = 1;
            format = "$icon $utilization";
            format_alt = "$icon $utilization $barchart";
          }

          {
            block = "temperature";
            chip = "*-isa-*";
            interval = 5;
            format = "$icon $max max";
            format_alt = "$icon $min min, $max max, $average avg";
          }

          {
            block = "memory";
            format = "$icon $mem_total_used";
            format_alt = "$icon_swap $swap_used";
          }

          {
            block = "disk_space";
            path = "/";
            format = "$icon $percentage";
            format_alt = "$icon $available $percentage";
          }

          {
            block = "net";
            device = "wlp58s0";
            format = "$icon $ssid";
            format_alt = "$icon $ssid $frequency $signal_strength $ip";
          }

          {
            block = "time";
            interval = 5;
            timezone = "Asia/Katmandu";
            # format = "$icon $timestamp.datetime(f:'%I:%M %P %Z', l:ne_NP)";
            format = "$icon $timestamp.datetime(f:'%I:%M %P', l:ne_NP)";
          }

          {
            block = "time";
            interval = 5;
            timezone = "Europe/Stockholm";
            # format = "$timestamp.datetime(f:'W%W %a %m/%d %H:%M %Z', l:sv_SE)";
            format = "$timestamp.datetime(f:'%H:%M %Z', l:sv_SE)";
          }

          {
            block = "toggle";
            format = "$icon ";
            command_state = "${pkgs.fish}/bin/fish -c get_xfce_theme | grep dark";
            command_on = "${pkgs.fish}/bin/fish -c toggle_xfce_theme";
            command_off = "${pkgs.fish}/bin/fish -c toggle_xfce_theme";
            interval = 5;
          }

          {
            block = "notify";
          }
        ];
      };
    };
  };
}
