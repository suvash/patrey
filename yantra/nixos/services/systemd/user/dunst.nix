{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    libnotify
    dunst
  ];

  systemd.user.services.dunst = {
    enable = true;
    description = "Dunst : Lightweight and customizable notification daemon";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
  };
}
