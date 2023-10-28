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

    outputs.homeManagerModules.git # configure
    outputs.homeManagerModules.fish # configure
    outputs.homeManagerModules.starship # configure

    # Or modules exported from other flakes (such as nix-colors):
    # inputs.nix-colors.homeManagerModules.default

    # You can also split up your configuration and import pieces of it here:
    # ./nvim.nix
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

  home.username = "suvash";
  home.homeDirectory = "/home/suvash";

  home.packages = with pkgs; [
    cmatrix
  ];

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
  programs.broot.enable = true; # configure
  programs.btop.enable = true; # configure

  programs.chromium.enable = true; # configure

  programs.dircolors.enable = false; # investigate
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.emacs.enable = true; # configure
  programs.exa.enable = true; # configure

  programs.feh.enable = true; # configure
  programs.firefox.enable = true; # configure

  programs.fuzzel.enable = false; # configure, wayland
  programs.fzf.enable = true; # configure

  programs.gallery-dl.enable = false; # investigate
  programs.gh.enable = true; # configure

  programs.gpg.enable = false; # investigate together with services

  programs.helix.enable = true; # configure
  programs.htop.enable = true; # configure

  programs.i3status.enable = false; # configure
  programs.i3status-rust.enable = false; # configure
  programs.info.enable = true;

  programs.jq.enable = true;

  programs.k9s.enable = false;
  programs.keychain.enable = false; # investigate
  programs.kitty.enable = true; # configure

  programs.less.enable = true;
  programs.lf.enable = true; # investigate
  programs.librewolf.enable = true; # investigate
  programs.lsd.enable = true; # investigate

  programs.man.enable = true;
  programs.mangohud.enable = true; # configure
  programs.mpv.enable = true; # configure

  programs.navi.enable = true; # investigate
  programs.neovim.enable = true; # configure
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
  programs.rofi.enable = true; # configure

  programs.sioyek.enable = true; # configure
  programs.skim.enable = true; # configure
  programs.sm64ex.enable = false; # configure

  programs.ssh.enable = false; # configure
  programs.swaylock.enable = false; # configure, sway

  programs.taskwarrior.enable = true; # configure
  programs.tealdeer.enable = true; # configure
  programs.tmux.enable = true; # configure
  programs.translate-shell.enable = true; # configure

  programs.vim = {
    enable = true;
    defaultEditor = true;
  };
  programs.vscode.enable = true; # configure

  programs.watson.enable = true; # configure
  programs.waybar.enable = false; # configure, sway
  programs.wlogout.enable = false; # configure, sway
  programs.wofi.enable = false; # configure, sway

  programs.yt-dlp.enable = true; # configure

  programs.zathura.enable = true; # configure
  programs.zellij.enable = true; # configure
  programs.zoxide.enable = true; # configure

  # Services

  services.autorandr.enable = true;
  services.avizo.enable = false; # configure, wayland

  services.batsignal.enable = true;
  services.betterlockscreen.enable = true;

  services.caffeine.enable = true;
  services.cbatticon.enable = true;
  services.clipman.enable = false; # configure, wayland
  services.clipmenu.enable = true;

  services.dunst.enable = true;

  services.easyeffects.enable = true;
  services.emacs.enable = false; # configure
  services.espanso.enable = false; # configure

  services.flameshot.enable = false; # configure
  services.fusuma.enable = false; # configure

  services.gammastep.enable = false; # configure
  services.gpg-agent.enable = false; # configure
  services.gromit-mpx.enable = false; # configure

  services.kanshi.enable = false; # configure, wayland

  services.mako.enable = false; # configure, wayland

  services.pasystray.enable = true; # pulseaudio
  services.picom.enable = true; # configure
  services.playerctld.enable = true;
  services.polybar.enable = false; # configure
  services.poweralertd.enable = true;
  services.pulseeffects.enable = false; # pulseaudio

  services.random-background.enable = false; # configure

  services.safeeyes.enable = true;
  services.screen-locker.enable = lib.mkForce false; # configure compare betterlockscreen
  services.stalonetray.enable = false; # configure
  services.swayidle.enable = false; # configure, sway
  services.sxhkd.enable = false; # configure instead of i3
  services.systembus-notify.enable = true;

  services.udiskie.enable = true; # configure
  services.unclutter.enable = true;

  services.volnoti.enable = true;

  services.wlsunset.enable = false; # configure, wayland

  services.xcape = {
    enable = true;
    mapExpression = {Control_L = "Escape";};
  };
  services.xidlehook.enable = false; # configure compare screenlocks
  services.xscreensaver.enable = false; # configure compare above
  services.xsettingsd.enable = false; # configure
  services.xsuspender.enable = false; # configure
}
