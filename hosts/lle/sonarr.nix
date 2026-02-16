{config, ...}: {
  services.sonarr = {
    enable = true;
    settings.server.port = 40200;
  };

  networking.firewall.allowedTCPPorts = [40200];

  services.cloudflared = {
    tunnels."lle".ingress = {
      "tv.hait.xyz" = "http://localhost:${toString 40200}";
    };
  };
}
