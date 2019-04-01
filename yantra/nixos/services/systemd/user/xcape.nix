{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    xcape
  ];

  systemd.user.services.xcape = {
    enable = true;
    description = "xcape : use CTRL as ESC when pressed alone";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStart = "${pkgs.xcape}/bin/xcape";
  };
}
