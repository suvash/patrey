{config, ...}: {
  services.navidrome = {
    enable = true;
    user = "navidrome";
    group = "navidrome";
    settings = {
      Address = "0.0.0.0";
      Port = 4533;
      MusicFolder = "/media/serve/music";
    };
  };

  users.users.navidrome = {
    extraGroups = ["${config.users.groups.media.name}"];
  };

  networking.firewall.allowedTCPPorts = [4533];

  services.cloudflared = {
    tunnels."lle-errbrr-com".ingress = {
      "mp.errbrr.com" = "http://localhost:${toString 4533}";
    };
  };
}
