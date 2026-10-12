{outputs, ...}:
{
  imports = [
    outputs.nixosModules.nfs_mounts
  ];

  services.nfsMounts.bhandaar = {
    server = "10.10.2.2";
    export = "/volume1/Bhandaar";
    mountPoint = "/mnt/nfs/bhakaari/bhandaar";
    readOnly = true;
    automount = true;
    nfsVersion = "4.0";
  };
}
