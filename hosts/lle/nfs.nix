{inputs, ...}: {
  boot.supportedFilesystems = ["nfs"];
  services.rpcbind.enable = true;

  systemd.mounts = [
    {
      type = "nfs";
      mountConfig = {
        Options = "noatime,nodiratime,rw";
      };
      what = "bhakaari.local:volume1/Downloads";
      where = "/mnt/bhakaari/downloads";
    }
  ];

  systemd.automounts = [
    {
      wantedBy = ["multi-user.target"];
      automountConfig = {
        TimeoutIdleSec = "600";
      };
      where = "/mnt/bhakaari/downloads";
    }
  ];
}
