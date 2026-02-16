{
  inputs,
  pkgs,
  ...
}: {
  imports = [
    inputs.sops-nix.nixosModules.sops
  ];

  sops.defaultSopsFile = ./sops/secrets.yaml;

  sops.age.generateKey = false;
  sops.age.sshKeyPaths = ["/etc/ssh/ssh_host_ed25519_key"];
  sops.gnupg.sshKeyPaths = [];

  environment.systemPackages = with pkgs; [
    age
    sops
    ssh-to-age
  ];
}
