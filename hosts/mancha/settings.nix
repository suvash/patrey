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
        default = "mancha";
        type = with lib.types; uniq str;
      };
      computername = lib.mkOption {
        default = "मञ्च";
        type = with lib.types; uniq str;
      };
      editor = lib.mkOption {
        default = "vim";
        type = with lib.types; uniq str;
      };
      patreydir = lib.mkOption {
        # relative to $HOME
        default = "Developer/patrey";
        type = with lib.types; uniq str;
      };
    };
  };
}
