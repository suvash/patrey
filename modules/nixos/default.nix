{
  avahi = import ./avahi.nix;
  pipewire = import ./pipewire.nix;
  pulseaudio = import ./pulseaudio.nix;
  yubikey = import ./yubikey.nix;
  nfs_mounts = import ./nfs_mounts.nix;
}
