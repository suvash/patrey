{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    xclip
    autocutsel
  ];

  systemd.user.services.autocutsel = {
    enable = true;
    description = "AutoCutSel tracks changes in various clipboard buffers";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig.Type = "forking";
    serviceConfig.Restart = "always";
    serviceConfig.RestartSec = 2;
    serviceConfig.ExecStartPre = "${pkgs.autocutsel}/bin/autocutsel -fork";
    serviceConfig.ExecStart = "${pkgs.autocutsel}/bin/autocutsel -selection PRIMARY -fork";
  };
}
