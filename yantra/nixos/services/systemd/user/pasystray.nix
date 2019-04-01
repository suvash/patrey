{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    pasystray
  ];

  systemd.user.services.pasystray = {
    enable = true;
    description = "PulseAudio system tray";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    environment.XDG_DATA_DIRS="/run/current-system/sw/share/";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.pasystray}/bin/pasystray";
  };
}
