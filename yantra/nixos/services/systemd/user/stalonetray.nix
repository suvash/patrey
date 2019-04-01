{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    stalonetray
  ];

  systemd.user.services.stalonetray = {
    enable = true;
    description = "Standalone tray";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.stalonetray}/bin/stalonetray";
  };
}
