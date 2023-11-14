{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    inputs.nix-index-database.hmModules.nix-index

    outputs.homeManagerModules.git
    outputs.homeManagerModules.fish
    outputs.homeManagerModules.starship

    ./settings.nix
    ./home-packages.nix

    ../../modules/home-manager/neovim.nix
    ../../modules/home-manager/tmux.nix
    ../../modules/home-manager/i3status.nix
  ];

  nixpkgs = {
    overlays = [
      # Add overlays your own flake exports (from overlays and pkgs dir):
      outputs.overlays.additions
      outputs.overlays.modifications
      outputs.overlays.unstable-packages
      outputs.overlays.master-packages
      outputs.overlays.sha-3be4a51-packages

      # You can also add overlays exported from other flakes:
      # neovim-nightly-overlay.overlays.default

      # Or define it inline, for example:
      # (final: prev: {
      #   hi = final.hello.overrideAttrs (oldAttrs: {
      #     patches = [ ./change-hello-to-hi.patch ];
      #   });
      # })
    ];
    # Configure your nixpkgs instance
    config = {
      # Disable if you don't want unfree packages
      allowUnfree = true;
      # Workaround for https://github.com/nix-community/home-manager/issues/2942
      allowUnfreePredicate = _: true;
    };
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";

  home.stateVersion = "23.05";

  home.username = "${config.settings.username}";
  home.homeDirectory = "/home/${config.settings.username}";

  home.sessionVariables = {
    EDITOR = "${config.settings.editor}";
    TERMINAL = "${config.settings.terminal}";
  };

  home.file.".screenshots/.keep".text = "";

  home.file.".wallpapers" = {
    source = ../../wallpapers/background;
    recursive = true;
  };

  xdg.userDirs = {
    enable = true;
    createDirectories = true;
    download = "${config.home.homeDirectory}/downloads";
    desktop = "${config.home.homeDirectory}/nope/Desktop";
    documents = "${config.home.homeDirectory}/nope/Documents";
    music = "${config.home.homeDirectory}/nope/Music";
    pictures = "${config.home.homeDirectory}/nope/Pictures";
    publicShare = "${config.home.homeDirectory}/nope/Public";
    templates = "${config.home.homeDirectory}/nope/Templates";
    videos = "${config.home.homeDirectory}/nope/Videos";
  };

  # Options to investigate
  # home
  # i18n
  # nix
  # pam
  # systemd.user
  # wayland
  # xdg
  # xfconf.settings
  # xsession

  programs.home-manager.enable = true;

  # things

  fonts.fontconfig.enable = true;

  gtk.enable = true; # configure
  qt.enable = true; # configure

  # Programs

  programs.alacritty.enable = true; # configure
  programs.autojump.enable = true; # configure
  programs.autorandr.enable = true; # configure

  programs.bash.enable = true; # configure
  programs.bashmount.enable = true;
  programs.bat.enable = true; # configure
  programs.bottom.enable = true; # configure
  programs.broot.enable = false; # configure
  programs.btop.enable = true; # configure

  programs.chromium.enable = true; # configure

  programs.dircolors.enable = false; # investigate
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.emacs.enable = true; # configure

  programs.feh.enable = true; # configure
  programs.firefox.enable = true; # configure

  programs.fuzzel.enable = false; # configure, wayland
  programs.fzf.enable = true; # configure

  programs.gh.enable = true; # configure

  programs.gpg.enable = false; # investigate together with services

  programs.helix.enable = true; # configure
  programs.htop.enable = true; # configure

  programs.info.enable = true;

  programs.jq.enable = true;

  programs.k9s.enable = false;
  programs.keychain.enable = false; # investigate

  programs.kitty = {
    enable = true;
    settings = {
      font_family = "Ubuntu Mono";
      font_size = "14";
      scrollback_lines = 100000;
      enable_audio_bell = false;
      hide_window_decorations = "yes";
    };
    extraConfig = ''
      modify_font underline_position 2px
      modify_font underline_thickness 2px
    '';
  };

  programs.less.enable = true;
  programs.lf.enable = true; # investigate
  programs.librewolf.enable = true; # investigate

  programs.man.enable = true;
  programs.mangohud.enable = true; # configure
  programs.mpv.enable = true; # configure

  programs.navi.enable = false; # investigate
  programs.newsboat.enable = false; # investigate
  programs.nix-index.enable = true; # investigate
  programs.nnn.enable = true; # investigate
  programs.noti.enable = true; # investigate
  programs.nushell.enable = true; # configure

  programs.obs-studio.enable = false; # configure

  programs.pandoc.enable = true; # configure
  programs.papis.enable = false; # investigate
  programs.pazi.enable = false; # investigate, compare to zoxide
  programs.pet.enable = false; # investigate
  programs.pistol.enable = true; # configure
  programs.pls.enable = true; # configure

  programs.qutebrowser.enable = true; # configure

  programs.readline.enable = true; # configure

  programs.rofi = {
    enable = true;
    font = "Ubuntu Mono 18";
    terminal = "${pkgs.kitty}/bin/kitty";
  };

  programs.sioyek.enable = true; # configure
  programs.skim.enable = false; # configure, compare fzf
  programs.sm64ex.enable = false; # configure

  programs.ssh.enable = false; # configure
  programs.swaylock.enable = false; # configure, sway

  programs.taskwarrior.enable = true; # configure
  programs.tealdeer = {
    enable = true;
    settings = {
      display.compact = true;
      updates.auto_update = true;
    };
  };

  programs.translate-shell.enable = true; # configure

  programs.vim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [base16-vim];
    extraConfig = ''
      if exists('$BASE16_THEME')
          \ && (!exists('g:colors_name')
          \ || g:colors_name != 'base16-$BASE16_THEME')
        let base16colorspace=256
        colorscheme base16-$BASE16_THEME
      endif
    '';
  };
  programs.vscode.enable = true; # configure

  programs.watson.enable = false; # configure
  programs.waybar.enable = false; # configure, sway
  programs.wlogout.enable = false; # configure, sway
  programs.wofi.enable = false; # configure, sway

  programs.yt-dlp.enable = true; # configure

  programs.zathura.enable = true; # configure
  programs.zellij.enable = true; # configure
  programs.zoxide.enable = false; # configure

  # Services

  services.autorandr.enable = true;
  services.avizo.enable = false; # configure, wayland

  services.batsignal.enable = true;
  services.blueman-applet.enable = true;

  services.caffeine.enable = true;
  services.cbatticon.enable = true;
  services.clipman.enable = false; # configure, wayland
  services.clipmenu.enable = true;

  services.dunst = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
      size = "16x16";
    };
  };

  services.easyeffects.enable = true;
  services.emacs.enable = false; # configure
  services.espanso.enable = false; # configure

  services.flameshot = {
    enable = true;
    settings = {
      General = {
        saveAsFileExtension = "png";
        disabledTrayIcon = true;
        showStartupLaunchMessage = false;
      };
    };
  };
  services.fusuma.enable = false; # configure

  services.gammastep.enable = false; # configure
  services.gpg-agent.enable = false; # configure
  services.gromit-mpx.enable = false; # configure

  services.kanshi.enable = false; # configure, wayland

  services.mako.enable = false; # configure, wayland

  services.pasystray.enable = true; # pulseaudio
  services.picom.enable = true; # configure
  services.playerctld.enable = true;

  services.poweralertd.enable = true;
  services.pulseeffects.enable = false; # pulseaudio

  services.random-background = {
    enable = true;
    display = "center";
    imageDirectory = "%h/.wallpapers";
    interval = "1h";
  };

  services.safeeyes.enable = true;
  services.screen-locker.enable = false;
  services.stalonetray.enable = false; # configure
  services.swayidle.enable = false; # configure, sway
  services.sxhkd.enable = false; # configure instead of i3 keybindings
  services.systembus-notify.enable = true;

  services.udiskie.enable = true; # configure
  services.unclutter.enable = true;

  services.volnoti.enable = true;

  services.wlsunset.enable = false; # configure, wayland

  services.xcape = {
    enable = true;
    mapExpression = {Control_L = "Escape";};
  };

  services.xidlehook = {
    enable = true;
    environment = {
      "PRIMARY_DISPLAY" = "$(xrandr | awk '/ primary/{print $1}')";
    };
    not-when-audio = true;
    not-when-fullscreen = true;
    timers = [
      {
        delay = 30;
        command = ''xrandr --output "$PRIMARY_DISPLAY" --brightness .5'';
        canceller = ''xrandr --output "$PRIMARY_DISPLAY" --brightness 1'';
      }
      {
        delay = 60;
        command = ''
          xrandr --output "$PRIMARY_DISPLAY" --brightness 1;${pkgs.i3lock}/bin/i3lock --nofork --ignore-empty-password --image /etc/wallpapers/lock.png'';
      }
      {
        delay = 1800;
        command = "systemctl suspend";
      }
    ];
  };
  services.xscreensaver.enable = false; # configure compare above
  services.xsettingsd.enable = false; # configure
  services.xsuspender.enable = false; # configure

  # Xsession
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
