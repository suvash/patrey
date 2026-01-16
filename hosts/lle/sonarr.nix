{config, ...}: {
  services.sonarr = {
    enable = true;
    user = "sonarr";
    group = "sonarr";
    settings.server.port = 40300;
  };

  users.users.sonarr = {
    extraGroups = ["${config.users.groups.media.name}"];
  };

  networking.firewall.allowedTCPPorts = [40300];

  services.cloudflared = {
    tunnels."lle".ingress = {
      "tv.hait.xyz" = "http://localhost:${toString 40300}";
    };
  };
}
