{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    networkmanagerapplet
    # Needed for icons
    hicolor_icon_theme
  ];

  systemd.user.services.nm-applet = {
    enable = true;
    description = "Network Manager applet system tray";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    environment.XDG_DATA_DIRS="/run/current-system/sw/share/";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.networkmanagerapplet}/bin/nm-applet";
  };
}
