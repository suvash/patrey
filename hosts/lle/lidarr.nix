{config, ...}: {
  services.lidarr = {
    enable = true;
    user = "lidarr";
    group = "lidarr";
    settings.server.port = 8686;
  };

  users.users.lidarr = {
    extraGroups = ["${config.users.groups.media.name}"];
  };

  networking.firewall.allowedTCPPorts = [8686];

  services.cloudflared = {
    tunnels."lle-errbrr-com".ingress = {
      "mz.errbrr.com" = "http://localhost:${toString 8686}";
    };
  };
}
