{
  lib,
  config,
  pkgs,
  ...
}: {
  users.users.hass = {
    extraGroups = ["${config.users.groups.dialout.name}"];
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
    customComponents = with pkgs.home-assistant-custom-components; [
      adaptive_lighting
    ];
    config = {
      # Includes dependencies for a basic setup
      # https://www.home-assistant.io/integrations/default_config/
      default_config = {};
      http = {
        server_host = "::";
        trusted_proxies = ["::1"];
        use_x_forwarded_for = true;
      };
      automation = [
        {
          id = "turn_off_living_room_kitchen_sunrise";
          alias = "Turn Off Living Room and Kitchen Lights After Sunrise";
          description = "Turns off lights in the living room and kitchen 15 minutes after sunrise.";
          trigger = [
            {
              platform = "sun";
              event = "sunrise";
              offset = "00:15:00";
            }
          ];
          condition = [];
          action = [
            {
              action = "light.turn_off";
              target = {
                area_id = [
                  "living_room"
                  "kitchen"
                ];
              };
              data = {
                transition = 5;
              };
            }
          ];
        }

        {
          id = "turn_off_bedroom_8am";
          alias = "Turn Off Bedroom Lights at 8 AM";
          description = "Automatically turns off all lights in the bedroom at exactly 8:00 AM daily.";
          trigger = [
            {
              platform = "time";
              at = "08:00:00";
            }
          ];
          condition = [];
          action = [
            {
              action = "light.turn_off";
              target = {
                area_id = [
                  "bedroom"
                ];
              };
              data = {
                transition = 5;
              };
            }
          ];
        }

        {
          id = "living_room_kitchen_night_dimming";
          alias = "Living Room and Kitchen Night Dimming Sequence";
          description = "";
          mode = "parallel";
          triggers = [
            {
              id = "dim_10";
              trigger = "time";
              at = "23:00:00";
            }
            {
              id = "dim_5";
              trigger = "time";
              at = "00:00:00";
            }
            {
              id = "turn_off";
              trigger = "time";
              at = "01:00:00";
            }
          ];
          conditions = [];
          actions = [
            {
              choose = [
                {
                  conditions = [
                    {
                      condition = "trigger";
                      id = [
                        "dim_10"
                      ];
                    }
                  ];
                  sequence = [
                    {
                      action = "light.turn_on";
                      data = {
                        brightness_pct = 10;
                        transition = 15;
                      };
                      target = {
                        area_id = [
                          "living_room"
                          "kitchen"
                        ];
                      };
                    }
                  ];
                }
                {
                  conditions = [
                    {
                      condition = "trigger";
                      id = [
                        "dim_5"
                      ];
                    }
                  ];
                  sequence = [
                    {
                      action = "light.turn_on";
                      data = {
                        brightness_pct = 5;
                        transition = 15;
                      };
                      target = {
                        area_id = [
                          "living_room"
                          "kitchen"
                        ];
                      };
                    }
                  ];
                }
                {
                  conditions = [
                    {
                      condition = "trigger";
                      id = [
                        "turn_off"
                      ];
                    }
                  ];
                  sequence = [
                    {
                      action = "light.turn_off";
                      data = {
                        transition = 15;
                      };
                      target = {
                        area_id = [
                          "living_room"
                          "kitchen"
                        ];
                      };
                    }
                  ];
                }
              ];
            }
          ];
        }

        {
          id = "kitchen_presence_night_light_combined";
          alias = "Kitchen Presence Night Light Control";
          description = "";
          mode = "restart";
          triggers = [
            {
              id = "presence_detected";
              trigger = "state";
              entity_id = [
                "binary_sensor.kitchen_presence"
              ];
              from = [
                "off"
              ];
              to = [
                "on"
              ];
            }
            {
              id = "presence_cleared";
              trigger = "state";
              entity_id = [
                "binary_sensor.kitchen_presence"
              ];
              from = [
                "on"
              ];
              for = {
                hours = 0;
                minutes = 1;
                seconds = 0;
              };
              to = [
                "off"
              ];
            }
          ];
          conditions = [];
          actions = [
            {
              choose = [
                {
                  conditions = [
                    {
                      condition = "trigger";
                      id = [
                        "presence_detected"
                      ];
                    }
                    {
                      condition = "time";
                      after = "22:00:00";
                      before = "08:00:00";
                    }
                    {
                      condition = "state";
                      entity_id = "light.hallway_lamp";
                      state = "off";
                    }
                  ];
                  sequence = [
                    {
                      action = "adaptive_lighting.set_manual_control";
                      data = {
                        entity_id = "switch.adaptive_lighting_adaptive_lighting";
                        lights = [
                          "light.hallway_lamp"
                        ];
                        manual_control = true;
                      };
                    }
                    {
                      action = "light.turn_on";
                      metadata = {};
                      data = {
                        brightness_pct = 5; # ~13 in 0-255 range
                        transition = 1;
                      };
                      target = {
                        entity_id = "light.hallway_lamp";
                      };
                    }
                  ];
                }
                {
                  conditions = [
                    {
                      condition = "trigger";
                      id = [
                        "presence_cleared"
                      ];
                    }
                    {
                      condition = "time";
                      after = "22:00:00";
                      before = "08:00:00";
                    }
                    {
                      condition = "numeric_state";
                      entity_id = "light.hallway_lamp";
                      attribute = "brightness";
                      above = 12;
                      below = 14;
                    }
                  ];
                  sequence = [
                    {
                      action = "light.turn_off";
                      metadata = {};
                      data = {
                        transition = 1;
                      };
                      target = {
                        entity_id = "light.hallway_lamp";
                      };
                    }
                    {
                      action = "adaptive_lighting.set_manual_control";
                      data = {
                        entity_id = "switch.adaptive_lighting_adaptive_lighting";
                        lights = [
                          "light.hallway_lamp"
                        ];
                        manual_control = false;
                      };
                    }
                  ];
                }
              ];
            }
          ];
        }
      ];
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
