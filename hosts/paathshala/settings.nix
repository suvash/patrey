{
  config,
  pkgs,
  lib,
  ...
}: {
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
        default = "paathshala";
        type = with lib.types; uniq str;
      };
      timezone_sthlm = lib.mkOption {
        default = "Europe/Stockholm";
        type = with lib.types; uniq str;
      };
      timezone_ktm = lib.mkOption {
        default = "Asia/Katmandu";
        type = with lib.types; uniq str;
      };
      terminal = lib.mkOption {
        default = "kitty";
        type = with lib.types; uniq str;
      };
      editor = lib.mkOption {
        default = "nvim";
        type = with lib.types; uniq str;
      };
      edtr = lib.mkOption {
        default = "vim";
        type = with lib.types; uniq str;
      };
    };
  };
}
