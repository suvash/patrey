{
  ...
}: {
  virtualisation.docker = {
    enable = false;
    rootless = {
      enable = true;
      setSocketVariable = true;
      daemon.settings.features.cdi = true;
    };
  };

  hardware.nvidia-container-toolkit.enable = true;
}
