{
  inputs,
  config,
  ...
}: {
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];

  sops.secrets = {
    "cloudflare/tunnels/lle-errbrr-com/creds.json" = {};
    "cloudflare/tunnels/lle-suva-sh/creds.json" = {};
  };

  services.cloudflared = {
    enable = true;
    tunnels = {
      "lle-errbrr-com" = {
        credentialsFile = "${config.sops.secrets."cloudflare/tunnels/lle-errbrr-com/creds.json".path}";
        ingress = {
          # fill this in each service individually
        };
        default = "http_status:404";
      };
      "lle-suva-sh" = {
        credentialsFile = "${config.sops.secrets."cloudflare/tunnels/lle-suva-sh/creds.json".path}";
        ingress = {
          # fill this in each service individually
        };
        default = "http_status:404";
      };
    };
  };
}
