{
  inputs,
  config,
  ...
}: {
  # ####################
  # config with secret's
  # but sabnzbd cannot update it

  # imports = [
  #   inputs.sops-nix.nixosModules.sops
  # ];

  # sops.secrets = {
  #   "sabnzbd/sabnzbd.ini" = {
  #     mode = "0440";
  #     group = config.users.groups.keys.name;
  #     restartUnits = ["sabnzbd.service"];
  #   };
  # };

  # services.sabnzbd = {
  #   enable = true;
  #   configFile = "/var/lib/sabnzbd/sabnzbd.ini";
  # };

  # same port in the config file
  # networking.firewall.allowedTCPPorts = [40000];

  # users.users.sabnzbd = {
  #   extraGroups = ["keys"]; # match the secret group above
  # };

  # # sabnzbd sets up directories based on config file location
  # systemd.tmpfiles.rules = [
  #   # "L" creates a symlink: L <path> <mode> <user> <group> <age> <argument>
  #   "L /var/lib/sabnzbd/sabnzbd.ini 0660 ${config.users.users.sabnzbd.name} ${config.users.users.sabnzbd.group} - ${config.sops.secrets."sabnzbd/sabnzbd.ini".path}"
  # ];

  # ####################
  # Simple config
  # update via sabnzbd gui or edit config file
  services.sabnzbd = {
    enable = true;
  };

  # open correct port
  networking.firewall.allowedTCPPorts = [40000];

  services.cloudflared = {
    tunnels."lle".ingress = {
      "sb.hait.xyz" = "http://localhost:${toString 40000}";
    };
  };
}
