{
  inputs,
  config,
  ...
}: {
  services.sabnzbd = {
    enable = true;
    user = "sabnzbd";
    group = "sabnzbd";
  };

  users.users.sabnzbd = {
    extraGroups = ["${config.users.groups.media.name}"];
  };

  networking.firewall.allowedTCPPorts = [40000];

  services.cloudflared = {
    tunnels."lle".ingress = {
      "sb.hait.xyz" = "http://localhost:${toString 40000}";
    };
  };
}
