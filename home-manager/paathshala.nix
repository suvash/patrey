{
  inputs,
  outputs,
  lib,
  config,
  pkgs,
  ...
}: {
  imports = [
    outputs.homeManagerModules.git
    outputs.homeManagerModules.fish
    outputs.homeManagerModules.starship

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

  programs.home-manager.enable = true;
  programs.git.enable = true;

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
