{
  description = "Suvash's NixOS Flake";

  nixConfig = {
    substituters = [
      "https://cache.nixos.org/"
    ];
    extra-substituters = [
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  inputs = {
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-2305.url = "github:nixos/nixpkgs/nixos-23.05";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs-2305";
    };

    alejandra.url = "github:kamadorueda/alejandra/3.0.0";
    alejandra.inputs.nixpkgs.follows = "nixpkgs-2305";
  };

  outputs = {
    nixpkgs-unstable,
    nixpkgs-2305,
    nixos-hardware,
    home-manager,
    alejandra,
    ...
  } @ inputs: let
    x86linux = "x86_64-linux";
  in {
    formatter.${x86linux} = alejandra.defaultPackage.${x86linux};

    # sudo nixos-rebuild switch --flake .#hostname
    nixosConfigurations = {
      paathshala = nixpkgs-2305.lib.nixosSystem rec {
        system = x86linux;
        specialArgs = {
          pkgs-unstable = import nixpkgs-unstable {
            inherit system;
            config.allowUnfree = true;
          };
          pkgs-stable = import nixpkgs-2305 {
            inherit system;
            config.allowUnfree = true;
          };
        };
        modules = [
          ./hosts/paathshala/configuration.nix
          nixos-hardware.nixosModules.dell-xps-13-9360
          nixos-hardware.nixosModules.common-gpu-intel

          ./modules/avahi.nix
        ];
      };
    };

    # First time : nix run home-manager/release-23.05 -- switch --flake .#username@hostname
    # Then after : home-manager switch --flake .#username@hostname
    homeConfigurations = {
      "suvash@paathshala" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs-2305.legacyPackages.${x86linux}; # required by home-manager
        modules = [./home.nix];
      };
    };
  };
}
