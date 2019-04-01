{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    xscreensaver
  ];

  systemd.user.services.xscreensaver = {
    enable = true;
    description = "X screensaver";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.xscreensaver}/bin/xscreensaver -no-splash";
  };
}
