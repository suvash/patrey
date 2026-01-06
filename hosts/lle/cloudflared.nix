{
  inputs,
  config,
  ...
}: {
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];

  sops.secrets = {
    "cloudflare/cert.pem" = {};
    "cloudflare/tunnels/lle/id" = {};
    "cloudflare/tunnels/lle/creds.json" = {};
  };

  services.cloudflared = {
    enable = true;
    certificateFile = "${config.sops.secrets."cloudflare/cert.pem".path}";
    tunnels = {
      "lle" = {
        credentialsFile = "${config.sops.secrets."cloudflare/tunnels/lle/creds.json".path}";
        ingress = {
          # fill this in each service individually
        };
        default = "http_status:404";
      };
    };
  };
}
