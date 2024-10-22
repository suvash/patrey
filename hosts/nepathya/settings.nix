{lib, ...}: {
  options = {
    settings = {
      username = lib.mkOption {
        default = "suvash";
        type = with lib.types; uniq str;
      };
      fullname = lib.mkOption {
        default = "Suvash Thapaliya";
        type = with lib.types; uniq str;
      };
      email = lib.mkOption {
        default = "suvash@gmail.com";
        type = with lib.types; uniq str;
      };
      hostname = lib.mkOption {
        default = "nepathya";
        type = with lib.types; uniq str;
      };
      computername = lib.mkOption {
        default = "नेपथ्य";
        type = with lib.types; uniq str;
      };
      editor = lib.mkOption {
        default = "vim";
        type = with lib.types; uniq str;
      };
    };
  };
}
