{config, pkgs, ...}:
{
  services.xserver.videoDrivers = [ "modesetting" "nvidia" ];

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  hardware.nvidia = {
    open = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    modesetting.enable = true;
    powerManagement.enable = true;
    nvidiaSettings = true;
  };

  nixpkgs.config = {
    allowUnfree = true;
    cudaSupport = true;
  };

  environment.systemPackages = with pkgs; [
    nvtopPackages.full

  ] ++ [

    # BLAS & CUDA enabled llama-cpp
    (pkgs.master.llama-cpp.override {
      blasSupport = true;
      cudaSupport = true;
      rocmSupport = false;
      metalSupport = false;
    })

  ];

}
