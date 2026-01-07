{config, ...}: {
  services.radarr = {
    enable = true;
    settings.server.port = 40100;
  };

  networking.firewall.allowedTCPPorts = [40100];

  services.cloudflared = {
    tunnels."lle".ingress = {
      "mv.hait.xyz" = "http://localhost:${toString 40100}";
    };
  };
}
