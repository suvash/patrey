{pkgs, ...}: {
  services.xcape = {
    enable = true;
    mapExpression = {Control_L = "Escape";};
  };
}
