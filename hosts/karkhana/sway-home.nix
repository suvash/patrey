{
  config,
  pkgs,
  lib,
  ...
}: let
  mod = "Mod4"; # Super / Windows key
  term = "${pkgs.foot}/bin/foot";
  launcher = "${pkgs.wofi}/bin/wofi --show drun";
  lock = "${pkgs.swaylock}/bin/swaylock --daemonize --color 1e1e2e";
in {
  # ── Packages ──────────────────────────────────────────────────────────────
  home.packages = with pkgs; [
    foot # fast Wayland-native terminal (default)
    wofi # application launcher (Wayland rofi)
    swaylock # screen locker
    swayidle # idle / suspend management
    waybar # customizable status bar
    mako # notification daemon
    grim # screenshots
    slurp # region selector (use with grim)
    wl-clipboard # clipboard (wl-copy / wl-paste)
    pavucontrol # GUI audio mixer
    swaybg # wallpaper setter
  ];

  # ── swaylock ──────────────────────────────────────────────────────────────
  programs.swaylock = {
    enable = true;
    settings = {
      color = "1e1e2e";
      font-size = 24;
      indicator-idle-visible = false;
      indicator-radius = 100;
      show-failed-attempts = true;
    };
  };

  # ── swayidle ──────────────────────────────────────────────────────────────
  services.swayidle = {
    enable = true;
    timeouts = [
      # Warn at 4m50s, lock at 5m, turn off display at 5m30s, suspend at 10m
      {
        timeout = 290;
        command = "${pkgs.libnotify}/bin/notify-send 'Locking in 10 seconds'";
      }
      {
        timeout = 300;
        command = lock;
      }
      {
        timeout = 330;
        command = "${pkgs.sway}/bin/swaymsg 'output * power off'";
        resumeCommand = "${pkgs.sway}/bin/swaymsg 'output * power on'";
      }
      {
        timeout = 600;
        command = "${pkgs.systemd}/bin/systemctl suspend";
      }
    ];
    events = [
      {
        event = "before-sleep";
        command = lock;
      }
      {
        event = "lock";
        command = lock;
      }
    ];
  };

  # ── mako (notifications) ──────────────────────────────────────────────────
  services.mako = {
    enable = true;
    settings = {
      # mako expects dash-separated keys
      default-timeout = 5000;
      background-color = "#1e1e2e";
      text-color = "#cdd6f4";
      border-color = "#89b4fa";
      border-radius = 8;
      border-size = 2;
      font = "Ubuntu Mono 10";
      margin = "10";
      padding = "12,16";
    };
  };

  # ── waybar ────────────────────────────────────────────────────────────────
  programs.waybar = {
    enable = true;
    # Waybar starts automatically via sway's startup block below.
    # Minimal config — extend to taste.
    settings = [
      {
        layer = "top";
        position = "top";
        height = 32;
        spacing = 4;

        modules-left = ["sway/workspaces" "sway/mode"];
        modules-center = ["clock"];
        modules-right = ["pulseaudio" "network" "cpu" "memory" "tray"];

        "sway/workspaces" = {
          disable-scroll = true;
          all-outputs = false;
        };

        clock = {
          format = " {:%a %d %b  %H:%M}";
          tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
        };

        cpu = {
          format = " {usage}%";
          tooltip = false;
        };

        memory = {
          format = " {}%";
        };

        network = {
          format-wifi = " {essid}";
          format-ethernet = " {ipaddr}";
          format-disconnected = "⚠ Disconnected";
          tooltip-format = "{ifname}: {ipaddr}/{cidr}";
        };

        pulseaudio = {
          format = "{icon} {volume}%";
          format-muted = " muted";
          format-icons = {default = ["" "" ""];};
          on-click = "${pkgs.pavucontrol}/bin/pavucontrol";
        };

        tray = {spacing = 8;};
      }
    ];

    style = ''
      * {
        font-family: "Ubuntu Mono", monospace;
        font-size: 12px;
        min-height: 0;
      }
      window#waybar {
        background: rgba(30, 30, 46, 0.92);
        color: #cdd6f4;
        border-bottom: 2px solid #313244;
      }
      #workspaces button {
        padding: 0 8px;
        color: #6c7086;
        border-radius: 0;
      }
      #workspaces button.focused,
      #workspaces button.active {
        color: #cdd6f4;
        border-bottom: 2px solid #89b4fa;
      }
      #workspaces button:hover {
        color: #cdd6f4;
        background: rgba(137, 180, 250, 0.15);
      }
      #clock, #cpu, #memory, #network, #pulseaudio, #tray {
        padding: 0 12px;
        color: #cdd6f4;
      }
      #battery.warning { color: #fab387; }
      #battery.critical { color: #f38ba8; }
    '';
  };

  # ── wofi ──────────────────────────────────────────────────────────────────
  programs.wofi = {
    enable = true;
    settings = {
      width = 600;
      height = 400;
      allow_markup = true;
      insensitive = true;
      prompt = "Search…";
    };
    style = ''
      window {
        background-color: #1e1e2e;
        border: 2px solid #313244;
        border-radius: 12px;
      }
      #input {
        background-color: #313244;
        color: #cdd6f4;
        border: none;
        border-radius: 8px;
        padding: 8px 12px;
        margin: 8px;
      }
      #entry {
        padding: 8px 12px;
        color: #cdd6f4;
      }
      #entry:selected {
        background-color: #45475a;
        border-radius: 6px;
      }
    '';
  };

  # ── Sway ──────────────────────────────────────────────────────────────────
  wayland.windowManager.sway = {
    enable = true;
    wrapperFeatures.gtk = true;

    config = rec {
      modifier = mod;
      terminal = term;
      menu = launcher;

      # Default font for window titles
      fonts = {
        names = ["Ubuntu Mono"];
        size = 12.0;
      };

      # ── Gaps (optional, remove if you prefer flush windows) ──────────────
      gaps = {
        inner = 6;
        outer = 4;
        smartGaps = true;
      };

      # ── Window borders ───────────────────────────────────────────────────
      window = {
        border = 2;
        titlebar = false;
      };

      floating = {
        border = 2;
        titlebar = false;
        modifier = mod;
      };

      # ── Colors (Catppuccin Mocha palette) ─────────────────────────────────
      colors = {
        focused = {
          border = "#89b4fa";
          background = "#1e1e2e";
          text = "#cdd6f4";
          indicator = "#89b4fa";
          childBorder = "#89b4fa";
        };
        unfocused = {
          border = "#313244";
          background = "#1e1e2e";
          text = "#6c7086";
          indicator = "#313244";
          childBorder = "#313244";
        };
        urgent = {
          border = "#f38ba8";
          background = "#1e1e2e";
          text = "#f38ba8";
          indicator = "#f38ba8";
          childBorder = "#f38ba8";
        };
      };

      # ── Startup programs ─────────────────────────────────────────────────
      startup = [
        {command = "${pkgs.waybar}/bin/waybar";}
        # Set a solid-colour wallpaper. Replace with swaybg -i /path/to/img.jpg
        {command = "${pkgs.swaybg}/bin/swaybg -c '#1e1e2e'";}
        # Clipboard history daemon (optional; remove if not wanted)
        # { command = "wl-paste --type text --watch cliphist store"; }
      ];

      # ── Input ─────────────────────────────────────────────────────────────
      input = {
        "type:keyboard" = {
          xkb_layout = "us";
          xkb_options = "caps:escape"; # remap CapsLock → Escape (vi-friendly)
          repeat_delay = "250";
          repeat_rate = "35";
        };
        "type:touchpad" = {
          tap = "enabled";
          natural_scroll = "enabled";
          dwt = "enabled"; # disable while typing
          middle_emulation = "enabled";
        };
      };

      # ── Keybindings ───────────────────────────────────────────────────────
      keybindings = lib.mkOptionDefault {
        # ─ Core ─
        "${mod}+Return" = "exec ${term}";
        "${mod}+d" = "exec ${launcher}";
        "${mod}+q" = "kill";
        "${mod}+ctrl+l" = "exec ${lock}";

        # ─ Focus (hjkl + arrows) ─
        "${mod}+h" = "focus left";
        "${mod}+j" = "focus down";
        "${mod}+k" = "focus up";
        "${mod}+l" = "focus right";
        "${mod}+Left" = "focus left";
        "${mod}+Down" = "focus down";
        "${mod}+Up" = "focus up";
        "${mod}+Right" = "focus right";

        # ─ Move windows ─
        "${mod}+Shift+h" = "move left";
        "${mod}+Shift+j" = "move down";
        "${mod}+Shift+k" = "move up";
        "${mod}+Shift+l" = "move right";

        # ─ Layout ─
        "${mod}+b" = "splith";
        "${mod}+v" = "splitv";
        "${mod}+s" = "layout stacking";
        "${mod}+w" = "layout tabbed";
        "${mod}+e" = "layout toggle split";
        "${mod}+f" = "fullscreen toggle";
        "${mod}+Shift+space" = "floating toggle";
        "${mod}+space" = "focus mode_toggle";
        "${mod}+a" = "focus parent";

        # ─ Screenshots (grim + slurp) ─
        "Print" = "exec ${pkgs.grim}/bin/grim ~/.screenshots/$(date +%s).png";
        "${mod}+Print" = "exec ${pkgs.grim}/bin/grim -g \"$(${pkgs.slurp}/bin/slurp)\" ~/.screenshots/$(date +%s).png";

        # ─ Volume (PipeWire / PulseAudio) ─
        "XF86AudioRaiseVolume" = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%+";
        "XF86AudioLowerVolume" = "exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-";
        "XF86AudioMute" = "exec wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle";
        "XF86AudioMicMute" = "exec wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle";

        # ─ Brightness (brightnessctl) ─
        "XF86MonBrightnessUp" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set +10%";
        "XF86MonBrightnessDown" = "exec ${pkgs.brightnessctl}/bin/brightnessctl set 10%-";

        # ─ Sway control ─
        "${mod}+Shift+c" = "reload";
        "${mod}+Shift+e" = "exec swaynag -t warning -m 'Exit sway?' -b 'Yes, exit' 'swaymsg exit'";

        # ─ Scratchpad ─
        "${mod}+Shift+minus" = "move scratchpad";
        "${mod}+minus" = "scratchpad show";
      };

      # ── Workspaces ────────────────────────────────────────────────────────
      # Switch to workspace
      # (Super+1..9 and Super+Shift+1..9 are provided by mkOptionDefault)
      workspaceOutputAssign = [
        {
          workspace = "1";
          output = "HDMI-A-2";
        }
        {
          workspace = "2";
          output = "HDMI-A-2";
        }
        {
          workspace = "3";
          output = "HDMI-A-2";
        }
        {
          workspace = "4";
          output = "HDMI-A-2";
        }
        {
          workspace = "5";
          output = "HDMI-A-2";
        }
        {
          workspace = "6";
          output = "DP-1";
        }
        {
          workspace = "7";
          output = "DP-1";
        }
        {
          workspace = "8";
          output = "DP-1";
        }
        {
          workspace = "9";
          output = "DP-1";
        }
        {
          workspace = "10";
          output = "DP-1";
        }
      ];

      # Modes
      modes = {
        resize = {
          "${mod}+r" = "mode default";
          h = "resize shrink width 10px";
          j = "resize grow height 10px";
          k = "resize shrink height 10px";
          l = "resize grow width 10px";
          Left = "resize shrink width 10px";
          Down = "resize grow height 10px";
          Up = "resize shrink height 10px";
          Right = "resize grow width 10px";
          Return = "mode default";
          Escape = "mode default";
        };
      };

      # ── Status bar ────────────────────────────────────────────────────────
      # We disable the built-in bar because waybar handles it.
      bars = [];
    };

    # Extra config not expressible via the Nix options above
    extraConfig = ''
      # ── Scaling rules  ─────────────────────────────────────────────────
      # DP-1 (4K) as primary at origin; HDMI-A-2 (1440p) left of it.
      output DP-1        resolution 3840x2160 scale 2 position 0,0
      output HDMI-A-2    resolution 2560x1440 scale 1.6 position -1600,120

      # ── Floating rules ─────────────────────────────────────────────────
      for_window [app_id="pavucontrol"]         floating enable
      for_window [app_id="nm-connection-editor"] floating enable
      for_window [title="Picture-in-Picture"]   floating enable, sticky enable

      # ── Include the critical NixOS dbus/portal config ──────────────────
      # This file is generated by the NixOS Sway module and sets up dbus
      # activation of portals.  DO NOT remove this line.
      include /etc/sway/config.d/*
    '';
  };

  # ── Sync environment variables into systemd user session ─────────────────
  # Fixes swayidle, mako, and other user services not seeing WAYLAND_DISPLAY.
  systemd.user.sessionVariables = {
    WAYLAND_DISPLAY = "$WAYLAND_DISPLAY";
    DISPLAY = "$DISPLAY";
    SWAYSOCK = "$SWAYSOCK";
    XDG_CURRENT_DESKTOP = "sway";
    WLR_DRM_DEVICES = "/dev/dri/intel-card";
  };
}
