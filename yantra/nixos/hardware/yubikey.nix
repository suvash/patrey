{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    yubikey-personalization
  ];

  services.pcscd.enable = true;

  services.udev.packages = with pkgs; [
    yubikey-personalization
  ];

}
