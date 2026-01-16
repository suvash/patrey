{config, ...}: {
  services.radarr = {
    enable = true;
    user = "radarr";
    group = "radarr";
    settings.server.port = 40200;
  };

  users.users.radarr = {
    extraGroups = ["${config.users.groups.media.name}"];
  };

  networking.firewall.allowedTCPPorts = [40200];

  services.cloudflared = {
    tunnels."lle".ingress = {
      "mv.hait.xyz" = "http://localhost:${toString 40200}";
    };
  };
}
