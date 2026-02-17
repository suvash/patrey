{lib, ...}: {
  options = {
    settings = {
      username = lib.mkOption {
        default = "suvash";
        type = with lib.types; uniq str;
      };
      hostname = lib.mkOption {
        default = "karkhana";
        type = with lib.types; uniq str;
      };
      timezone_sthlm = lib.mkOption {
        default = "Europe/Stockholm";
        type = with lib.types; uniq str;
      };
    };
  };
}

