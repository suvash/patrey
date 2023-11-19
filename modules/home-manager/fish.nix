{
  inputs,
  pkgs,
  ...
}: {
  programs.fish = {
    enable = true;

    interactiveShellInit = ''
      set -Ux BASE16_FZF_PATH "${inputs.base16-fzf}"
      set -Ux BASE16_SHELL_PATH "${inputs.base16-shell}"
      source "$BASE16_SHELL_PATH/profile_helper.fish"
    '';

    functions = {
      getignore = "curl -sL https://www.gitignore.io/api/$argv";
      configure_xfce = {
        description = "set xfce settings (custom lock command, related settings)";
        body = ''
          # custom lock command to be used by xfce
          xfconf-query --create -c xfce4-session -p /general/LockCommand -t string -s "${pkgs.i3lock}/bin/i3lock --nofork --ignore-empty-password --show-failed-attempts --image /etc/wallpapers/lock.png";
          # lock screen on suspend and hibernate
          xfconf-query -c xfce4-session -p /shutdown/LockScreen -s true
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/lock-screen-suspend-hibernate -s true
          # disable saved sessions
          xfconf-query -c xfce4-session -p /general/SaveOnExit -s false
          # lid action on ac : suspend
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/lid-action-on-ac -s 1
          # lid action on battery : hibernate
          xfconf-query -c xfce4-power-manager -p /xfce4-power-manager/lid-action-on-battery -s 2
        '';
      };
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
          eval $theme_target
          set body "Switching to $target theme"
          if set -q DISPLAY; and set -q XDG_SESSION_TYPE; and test "$XDG_SESSION_TYPE" = "x11"
            set subject "Toggling light/dark SHELL theme"
            ${pkgs.libnotify}/bin/notify-send --urgency=NORMAL $subject $body
          else
            echo $body
          end
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
      # listing
      "..." = "../..";
      "...." = "../../..";
      "....." = "../../../..";
      p = "cd $PATREY_PATH";
      b = "bat";
      v = "vim (fzf --preview 'bat --number --color=always {}')";
      n = "nvim";
      ll = "ls -lah";
      lsd = "tree --dirsfirst -ChF -L 1";

      # nix things
      "nrs" = "sudo nixos-rebuild switch --flake $PATREY_PATH#(hostname)";
      "hms" = "home-manager switch --flake $PATREY_PATH#(whoami)@(hostname)";

      # git things
      gu = "gitui";
      ga = "git add";
      gs = "git show --stat";
      gst = "git status -sb";
      gd = "git diff";
      gdc = "git diff --cached";
      gp = "git push";
      gpf = "git push --force-with-lease";
      gpu = "git push --set-upstream origin (git branch --show-current)";
      gpuf = "git push --set-upstream origin (git branch --show-current) --force-with-lease";
      gpd = "git push --delete origin (git branch --show-current)";
      gsi = "git submodule init";
      gsu = "git submodule update";
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
      gl = "git log --oneline --decorate=full";
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
      dmsg = "dmesg -w -L";

      # other
      du = "du -chs *";
      dg = "dig +noall +answer";
      fonts = "fc-list : family";
      httpserve = "python -m http.server 7531";
      kssh = "kitty +kitten ssh";
      myip = "curl ifconfig.me/ip";

      # fish functions
      tst = "toggle_shell_theme";
    };
  };
}
