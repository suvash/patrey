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
      "mv.errbrr.com" = "http://localhost:${toString 40200}";
    };
  };
}
