{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    unstable.udiskie
  ];

  systemd.user.services.udiskie = {
    enable = true;
    description = "Udiskie : Automounter for removable media";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.unstable.udiskie}/bin/udiskie -A -n -s -F";
  };
}
