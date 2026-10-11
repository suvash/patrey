{pkgs, ...}: {
  services.immich = {
    package = pkgs.unstable.immich;
    enable = true;
    host = 0.0.0.0;
    port = 2283;
    openFirewall = true;
    environment = {
      IMMICH_LOG_LEVEL = "warn";
    };
  };

  services.cloudflared = {
    tunnels."lle".ingress = {
      "im.errbrr.com" = "http://localhost:${toString 2283}";
    };
  };
}
