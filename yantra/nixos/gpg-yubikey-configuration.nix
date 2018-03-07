{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    gnupg
    yubikey-personalization
  ];

  services.pcscd.enable = true;

  services.udev.packages = with pkgs; [
    yubikey-personalization
  ];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

}
