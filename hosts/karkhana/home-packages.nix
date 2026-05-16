{
  pkgs,
  inputs,
  ...
}: {
  home.packages = with pkgs; [
    openssl
    gnumake
  ];
}
