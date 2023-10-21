{
  description = "Suvash's NixOS Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, ... }@inputs:
    let
      system = "x86_64-linux";
    in
    {
      # sudo nixos-rebuild switch --flake .#hostname
      nixosConfigurations = {
        paathshala = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./hosts/paathshala/configuration.nix
          ];
        };
      };

      # First time : nix run home-manager/release-23.05 -- switch --flake .#username@hostname
      # Then after : home-manager switch --flake .#username@hostname
      homeConfigurations = {
        "suvash@paathshala" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system}; # required by home-manager
          modules = [ ./home.nix ];
        };
      };

    };
}
