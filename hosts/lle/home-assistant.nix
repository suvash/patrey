{
  lib,
  config,
  ...
}: {
  users.users.hass = {
    extraGroups = ["dialout"];
  };

  systemd.services.home-assistant.serviceConfig = {
    DevicePolicy = lib.mkForce "auto"; # to allow all character devices
    SupplementaryGroups = lib.mkForce ["dialout"];
    ReadWritePaths = lib.mkAfter ["/dev"];
  };

  services.home-assistant = {
    enable = true;
    extraComponents = [
      # Components required to complete the onboarding
      "analytics"
      "google_translate"
      "met"
      "radio_browser"
      "shopping_list"
      # Recommended for fast zlib compression
      # https://www.home-assistant.io/integrations/isal
      "isal"
      # Additional
      "homekit_controller"
      "homeassistant_connect_zbt2"
      "zha"
      "apple_tv"
      "synology_dsm"
      "thread"
    ];
    config = {
      # Includes dependencies for a basic setup
      # https://www.home-assistant.io/integrations/default_config/
      default_config = {};
      http = {
        server_host = "::1";
        trusted_proxies = ["::1"];
        use_x_forwarded_for = true;
      };
    };
  };

  networking.firewall.allowedTCPPorts = [
    config.services.home-assistant.config.http.server_port
  ];

  services.cloudflared = {
    tunnels."lle".ingress = {
      "ha.hait.xyz" = "http://localhost:${toString config.services.home-assistant.config.http.server_port}";
    };
  };
}
