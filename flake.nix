{
  description = "Suvash's NixOS Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs2305.url = "github:nixos/nixpkgs/nixos-23.05";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs2305";
    };
  };

  outputs = { nixpkgs, nixpkgs2305, nixos-hardware, home-manager, ... }@inputs:
    let
      x86linux = "x86_64-linux";
    in
    {
      # sudo nixos-rebuild switch --flake .#hostname
      nixosConfigurations = {
        paathshala = nixpkgs.lib.nixosSystem {
          system = x86linux;
          modules = [
            ./hosts/paathshala/configuration.nix
            nixos-hardware.nixosModules.dell-xps-13-9360
            nixos-hardware.nixosModules.common-gpu-intel
          ];
        };
      };

      # First time : nix run home-manager/release-23.05 -- switch --flake .#username@hostname
      # Then after : home-manager switch --flake .#username@hostname
      homeConfigurations = {
        "suvash@paathshala" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${x86linux}; # required by home-manager
          modules = [ ./home.nix ];
        };
      };

    };
}
