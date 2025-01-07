{pkgs, ...}: {
  programs.fish = {
    enable = true;

    interactiveShellInit = ''
    '';

    functions = {
      getignore = "curl -sL https://www.gitignore.io/api/$argv";
      configure_xfce = {
        description = "set xfce settings (custom lock command, related settings)";
        body = ''
          # custom lock command to be used by xfce
          xfconf-query --create -c xfce4-session -p /general/LockCommand -t string -s "${pkgs.i3lock}/bin/i3lock --ignore-empty-password --show-failed-attempts --image /etc/wallpapers/lock.png";
          # xfce icon theme
          xfconf-query -c xsettings -p /Net/IconThemeName -s Adwaita
          # lock screen on suspend and hibernate
          xfconf-query -c xfce4-session -p /shutdown/LockScreen -s true
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/lock-screen-suspend-hibernate -s true
          # disable saved sessions
          xfconf-query -c xfce4-session -p /general/SaveOnExit -s false
          # show power tray icon
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/show-tray-icon -s true
          # brightness step count
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/brightness-step-count -s 20
          # power button action : ask
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/power-button-action -s 3
          # critical power level : 5
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/critical-power-level -s 5
          # critical power action on battery : hibernate
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/critical-power-action -s 2
          # inactivity sleep mode on battery activate in minutes
          #
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/inactivity-on-battery -s 10
          # inactivity sleep mode on battery : suspend
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/inactivity-sleep-mode-on-battery -s 1
          # lid action on battery : hibernate
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/lid-action-on-battery -s 1
          # brightness reduce on battery after seconds
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/brightness-on-battery -s 15
          # brightness reduce level on battery
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/brightness-level-on-battery -s 20
          #
          # inactivity sleep mode on ac activate in minutes
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/inactivity-on-ac -s 20
          # inactivity sleep mode on ac : suspend
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/inactivity-sleep-mode-on-ac -s 1
          # lid action on ac : hibernate
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/lid-action-on-ac -s 1
          # brightness reduce on ac after seconds
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/brightness-on-ac -s 15
          # brightness reduce level on ac
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/brightness-level-on-ac -s 80
          #
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/dpms-enabled -s true
          # display blank on battery after minutes
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/blank-on-battery -s 0
          # display sleep on battery after minutes
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/dpms-on-battery-sleep -s 1
          # display off on battery after minutes
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/dpms-on-battery-off -s 3
          # display blank on ac after minutes
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/blank-on-ac -s 0
          # display sleep on ac after minutes
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/dpms-on-ac-sleep -s 5
          # display off on ac after minutes
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/dpms-on-ac-off -s 10
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
      set_gtk_theme = {
        description = "set a gtk theme";
        body = ''
          set theme $argv
          set command "gsettings set org.gnome.desktop.interface gtk-theme $theme"
          eval $command
        '';
      };
      set_gtk_colorscheme = {
        description = "set a gtk colorscheme";
        body = ''
          set colorscheme $argv
          set command "gsettings set org.gnome.desktop.interface color-scheme $colorscheme"
          eval $command
        '';
      };
      toggle_light_dark_theme = {
        description = "toggle between dark & light theme";
        body = ''
          set theme_light "Adwaita"
          set colorscheme_light "prefer-light"
          set theme_dark "Adwaita-dark"
          set colorscheme_dark "prefer-dark"
          set theme_current (get_xfce_theme)
          set theme_target ""
          set color_scheme_target ""
          switch $theme_current
          case $theme_light
            set theme_target $theme_dark
            set colorscheme_target $colorscheme_dark
          case $theme_dark
            set theme_target $theme_light
            set colorscheme_target $colorscheme_light
          case "*"
            set theme_target $theme_dark
            set colorscheme_target $colorscheme_dark
          end
          set subject "Toggling light/dark XFCE theme"
          set body "Switching to $theme_target theme"
          ${pkgs.libnotify}/bin/notify-send --urgency=NORMAL $subject $body
          set_xfce_theme $theme_target
          set_gtk_theme $theme_target
          set_gtk_colorscheme $colorscheme_target
        '';
      };
    };

    shellAbbrs = {
      # listing
      "..." = "../..";
      "...." = "../../..";
      "....." = "../../../..";
      p = "cd $PATREY_PATH";
      b = "bat";
      v = "vim (fzf --color 16 --preview 'bat --number --color=always {}')";
      n = "nvim";
      ll = "ls -lah";
      lsd = "tree --dirsfirst -ChF -L 1";

      # nix things
      "nrs" = "sudo nixos-rebuild switch --flake $PATREY_PATH#(hostname)";
      "drs" = "darwin-rebuild switch --flake $PATREY_PATH#(hostname)";
      "hms" = "home-manager switch --flake $PATREY_PATH#(whoami)@(hostname)";

      # git things
      gu = "gitui";
      ga = "git add";
      gap = "git add --patch";
      gs = "git show --stat";
      gst = "git status -sb";
      gd = "git diff";
      gdc = "git diff --cached";
      gp = "git push";
      gpf = "git push --force-with-lease";
      gpu = "git push --set-upstream origin (git branch --show-current)";
      gpuf = "git push --set-upstream origin (git branch --show-current) --force-with-lease";
      gpd = "git push --delete origin (git branch --show-current)";
      gup = "git fetch --all --prune --prune-tags; and git rebase --rebase-merges '@{upstream}'";
      gsp = "git stash; and git fetch --all -p; and git rebase --rebase-merges '@{upstream}'; and git stash pop";
      gprn = "git remote prune origin --dry-run";
      gm = "git merge --no-ff --log";
      gc = "git commit -v";
      gca = "git commit -v --amend";
      gco = "git checkout";
      gcob = "git checkout -b";
      gcm = "git checkout main";
      gcms = "git checkout master";
      gb = "git branch";
      gbr = "git branch -r";
      gbd = "git branch --delete";
      gbdf = "git branch --delete --force";
      gcp = "git cherry-pick";
      gl = "git log --oneline --graph --decorate=full";
      grhh = "git reset HEAD --hard";
      gcln = "git clean -f -d";
      gdmlb = "git branch --merged | grep -v '*' | xargs -n 1 git branch -d";
      grbm = "git rebase -i (git show-branch --merge-base main)";
      grbms = "git rebase -i (git show-branch --merge-base master)";

      # docker things
      di = "docker image ls";
      dc = "docker container ls";
      dn = "docker network ls";
      dv = "docker volume ls";
      ds = "docker system df";
      dprn = "docker system prune --volumes";

      # tmux things
      t = "tmux new";
      ta = "tmux attach";
      tls = "tmux list-sessions";

      # gpg things
      gpgrst = "gpg-connect-agent --quiet updatestartuptty /bye";

      # monitor
      p1 = "ping 1.1.1.1";
      pcf = "ping cfl.re";
      pgg = "ping goo.gl";
      ports = "netstat -tunap";

      # system
      h = "hostname";
      dmsg = "dmesg -w -L";
      battery = "upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep time";

      # APP_ENV
      devenv = "echo 'APP_ENV=development' > .env";
      stgenv = "echo 'APP_ENV=staging' > .env";

      # other
      du = "du -chs *";
      dg = "dig +noall +answer";
      fonts = "fc-list : family";
      httpserve = "python -m http.server 7531";
      kssh = "kitty +kitten ssh";
      myip = "curl ifconfig.me/ip";

      # fish functions
      tt = "toggle_light_dark_theme";
    };
  };
}
