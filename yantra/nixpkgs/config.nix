{
  allowUnfree = true;
  packageOverrides = pkgs_: with pkgs_; {
    yantra = with pkgs; buildEnv {
      name = "yantra";
      paths = [
        ncdu
        bashmount
      ];
    };
  };
}
