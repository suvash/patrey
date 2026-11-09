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
      "datetime"
      "homekit_controller"
      "homeassistant_connect_zbt2"
      "zha"
      "apple_tv"
      "synology_dsm"
      "thread"
      # marantz avr
      "denonavr"
      "heos"
      "music_assistant"

      # required by ha-tapo-control
      "ffmpeg"
      "onvif"
      "stream"
      "network"
    ];
    customComponents = with pkgs.home-assistant-custom-components; [
      adaptive_lighting

      (pkgs.callPackage ./ha-tapo-control.nix {})
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
      template = [
        {
          binary_sensor = [
            {
              name = "Hallway activity";
              # Hallway is "active" when either neighbour is; delay_off is the hallway's own timeout
              state = ''
                {{ is_state('binary_sensor.kitchen_motion', 'on')
                   or is_state('binary_sensor.living_room_presence', 'on') }}
              '';
              delay_off = "00:05:00";
            }
          ];
        }
      ];
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
                        transition = 5;
                      };
                      target = {
                        entity_id = ''
                          {{ (area_entities('living_room') + area_entities('kitchen'))
                             | select('match', 'light\.')
                             | select('is_state', 'on')
                             | list }}
                        '';
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
                        transition = 5;
                      };
                      target = {
                        entity_id = ''
                          {{ (area_entities('living_room') + area_entities('kitchen'))
                             | select('match', 'light\.')
                             | select('is_state', 'on')
                             | list }}
                        '';
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
                        transition = 5;
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
          id = "living_room_presence_night_light";
          alias = "Living Room Presence Night Light";
          description = "";
          mode = "restart";
          triggers = [
            {
              id = "presence_detected";
              trigger = "state";
              entity_id = ["binary_sensor.living_room_presence"];
              from = ["off"];
              to = ["on"];
            }
            {
              id = "presence_cleared";
              trigger = "state";
              entity_id = ["binary_sensor.living_room_presence"];
              from = ["on"];
              to = ["off"];
              for = {
                hours = 0;
                minutes = 5;
                seconds = 0;
              };
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
                      id = ["presence_detected"];
                    }
                    {
                      condition = "time";
                      after = "20:30:00";
                      before = "08:00:00";
                    }
                    {
                      condition = "state";
                      entity_id = "light.living_room_floor_lamp";
                      state = "off";
                    }
                  ];
                  sequence = [
                    {
                      action = "adaptive_lighting.set_manual_control";
                      data = {
                        entity_id = "switch.adaptive_lighting_adaptive_lighting";
                        lights = ["light.living_room_floor_lamp"];
                        manual_control = true;
                      };
                    }
                    {
                      action = "light.turn_on";
                      metadata = {};
                      data = {
                        brightness_pct = 14;
                        transition = 1;
                      }; # ~36 in 0-255
                      target = {entity_id = "light.living_room_floor_lamp";};
                    }
                  ];
                }
                {
                  conditions = [
                    {
                      condition = "trigger";
                      id = ["presence_cleared"];
                    }
                    {
                      condition = "numeric_state";
                      entity_id = "light.living_room_floor_lamp";
                      attribute = "brightness";
                      above = 34;
                      below = 38;
                    }
                  ];
                  sequence = [
                    {
                      action = "light.turn_off";
                      metadata = {};
                      data = {transition = 1;};
                      target = {entity_id = "light.living_room_floor_lamp";};
                    }
                    {
                      action = "adaptive_lighting.set_manual_control";
                      data = {
                        entity_id = "switch.adaptive_lighting_adaptive_lighting";
                        lights = ["light.living_room_floor_lamp"];
                        manual_control = false;
                      };
                    }
                  ];
                }
              ];
            }
          ];
        }

        {
          id = "kitchen_motion_night_light";
          alias = "Kitchen Motion Night Light";
          description = "";
          mode = "restart";
          triggers = [
            {
              id = "motion_detected";
              trigger = "state";
              entity_id = ["binary_sensor.kitchen_motion"];
              from = ["off"];
              to = ["on"];
            }
            {
              id = "motion_cleared";
              trigger = "state";
              entity_id = ["binary_sensor.kitchen_motion"];
              from = ["on"];
              to = ["off"];
              for = {
                hours = 0;
                minutes = 5;
                seconds = 0;
              };
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
                      id = ["motion_detected"];
                    }
                    {
                      condition = "numeric_state";
                      entity_id = "sensor.kitchen_motion_illuminance";
                      below = 15;
                    }
                    {
                      condition = "state";
                      entity_id = "light.kitchen_lamp";
                      state = "off";
                    }
                  ];
                  sequence = [
                    {
                      action = "adaptive_lighting.set_manual_control";
                      data = {
                        entity_id = "switch.adaptive_lighting_adaptive_lighting";
                        lights = ["light.kitchen_lamp"];
                        manual_control = true;
                      };
                    }
                    {
                      action = "light.turn_on";
                      metadata = {};
                      data = {
                        brightness_pct = 18;
                        transition = 1;
                      }; # ~46 in 0-255
                      target = {entity_id = "light.kitchen_lamp";};
                    }
                  ];
                }
                {
                  conditions = [
                    {
                      condition = "trigger";
                      id = ["motion_cleared"];
                    }
                    {
                      condition = "numeric_state";
                      entity_id = "light.kitchen_lamp";
                      attribute = "brightness";
                      above = 44;
                      below = 48;
                    }
                  ];
                  sequence = [
                    {
                      action = "light.turn_off";
                      metadata = {};
                      data = {transition = 1;};
                      target = {entity_id = "light.kitchen_lamp";};
                    }
                    {
                      action = "adaptive_lighting.set_manual_control";
                      data = {
                        entity_id = "switch.adaptive_lighting_adaptive_lighting";
                        lights = ["light.kitchen_lamp"];
                        manual_control = false;
                      };
                    }
                  ];
                }
              ];
            }
          ];
        }

        {
          id = "hallway_night_light";
          alias = "Hallway Night Light";
          description = "";
          mode = "restart";
          triggers = [
            {
              id = "active";
              trigger = "state";
              entity_id = ["binary_sensor.hallway_activity"];
              from = ["off"];
              to = ["on"];
            }
            {
              id = "idle";
              trigger = "state";
              entity_id = ["binary_sensor.hallway_activity"];
              from = ["on"];
              to = ["off"];
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
                      id = ["active"];
                    }
                    {
                      condition = "time";
                      after = "20:30:00";
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
                        lights = ["light.hallway_lamp"];
                        manual_control = true;
                      };
                    }
                    {
                      action = "light.turn_on";
                      metadata = {};
                      data = {
                        brightness_pct = 16;
                        transition = 1;
                      }; # ~41 in 0-255
                      target = {entity_id = "light.hallway_lamp";};
                    }
                  ];
                }
                {
                  conditions = [
                    {
                      condition = "trigger";
                      id = ["idle"];
                    }
                    {
                      condition = "numeric_state";
                      entity_id = "light.hallway_lamp";
                      attribute = "brightness";
                      above = 39;
                      below = 43;
                    }
                  ];
                  sequence = [
                    {
                      action = "light.turn_off";
                      metadata = {};
                      data = {transition = 1;};
                      target = {entity_id = "light.hallway_lamp";};
                    }
                    {
                      action = "adaptive_lighting.set_manual_control";
                      data = {
                        entity_id = "switch.adaptive_lighting_adaptive_lighting";
                        lights = ["light.hallway_lamp"];
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
    tunnels."lle-errbrr-com".ingress = {
      "ha.errbrr.com" = "http://localhost:${toString config.services.home-assistant.config.http.server_port}";
    };
  };
}
