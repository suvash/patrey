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
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";
    nixpkgs-3be4a51.url = "github:nixos/nixpkgs/3be4a51a23edfa3a3c4ceabe25328520dd1d9fd4";

    nixos-hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };

    alejandra.url = "github:kamadorueda/alejandra/3.0.0";
    alejandra.inputs.nixpkgs.follows = "nixpkgs-stable";
  };

  outputs = {
    self,
    nixpkgs-unstable,
    nixpkgs-stable,
    nixos-hardware,
    home-manager,
    alejandra,
    ...
  } @ inputs: let
    inherit (self) outputs;
    # Supported systems for your flake packages, shell, etc.
    x86linux = "x86_64-linux";
    x86darwin = "x86_64-darwin";
    systems = [
      x86linux
      x86darwin
    ];
    forAllSystems = nixpkgs-stable.lib.genAttrs systems;
  in {
    # Custom packages, available through 'nix build', 'nix shell', etc
    packages = forAllSystems (system: import ./pkgs nixpkgs-stable.legacyPackages.${system});
    # nix fmt
    formatter = forAllSystems (system: alejandra.defaultPackage.${system});
    # overlays
    overlays = import ./overlays {inherit inputs;};
    # shareable non personal nixos modules
    nixosModules = import ./modules/nixos;
    # shareable non personal home manger modules
    homeManagerModules = import ./modules/home-manager;

    # sudo nixos-rebuild switch --flake .#hostname
    nixosConfigurations = {
      paathshala = nixpkgs-stable.lib.nixosSystem rec {
        system = x86linux;
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./hosts/paathshala/configuration.nix
        ];
      };
    };

    # First time : nix run home-manager/release-23.05 -- switch --flake .#username@hostname
    # Then after : home-manager switch --flake .#username@hostname
    homeConfigurations = {
      "suvash@paathshala" = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs-stable.legacyPackages.${x86linux}; # required by home-manager
        extraSpecialArgs = {inherit inputs outputs;};
        modules = [./home-manager/paathshala.nix];
      };
    };
  };
}
