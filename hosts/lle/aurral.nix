{config, ...}: {
  services.aurral = {
    enable = true;
    port = 3001;
    user = "aurral";
    group = "aurral";
    directories = [
      "/media/serve/music"
    ];
  };

  users.users.aurral = {
    extraGroups = ["${config.users.groups.media.name}"];
  };

  networking.firewall.allowedTCPPorts = [3001];

  services.cloudflared = {
    tunnels."lle-errbrr-com".ingress = {
      "ar.errbrr.com" = "http://localhost:${toString 3001}";
    };
  };
}
