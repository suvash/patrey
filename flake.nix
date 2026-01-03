{
  description = "Suvash's NixOS+Darwin Flakes";

  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://suvash.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "suvash.cachix.org-1:ZJaRn/gUWxarb/rtYiP3njBLUBw+JYpKSg9dDS0NKjM="
    ];
  };

  inputs = {
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/nixos-25.11";
    nixpkgs-master.url = "github:nixos/nixpkgs/master";

    nixpkgs-darwin-stable.url = "github:nixos/nixpkgs/nixpkgs-25.11-darwin";
    nix-darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-25.11";
      inputs.nixpkgs.follows = "nixpkgs-darwin-stable";
    };

    nixos-hardware.url = "github:nixos/nixos-hardware";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };

    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs-stable";
    };

    lsp-zero-nvim-3 = {
      url = "github:VonHeikemen/lsp-zero.nvim/v3.x";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs-unstable,
    nixpkgs-stable,
    home-manager,
    nixpkgs-darwin-stable,
    nix-darwin,
    ...
  } @ inputs: let
    inherit (self) outputs;
    # Supported systems for your flake packages, shell, etc.
    x86linux = "x86_64-linux";
    x86darwin = "x86_64-darwin";
    systems = [x86linux x86darwin];
    forAllSystems = nixpkgs-stable.lib.genAttrs systems;
  in {
    # Custom packages, available through 'nix build', 'nix shell', etc
    packages =
      forAllSystems
      (system: import ./pkgs nixpkgs-stable.legacyPackages.${system});
    # nix fmt
    formatter =
      forAllSystems
      (system: nixpkgs-unstable.legacyPackages.${system}.alejandra);
    # overlays
    overlays = import ./overlays {inherit inputs;};
    # shareable nixos modules
    nixosModules = import ./modules/nixos;
    # shareable darwin modules
    darwinModules = import ./modules/darwin;
    # shareable home manager modules
    homeManagerModules = import ./modules/home-manager;

    # sudo nixos-rebuild switch --flake .#hostname
    nixosConfigurations = {
      paathshala = nixpkgs-stable.lib.nixosSystem rec {
        system = x86linux;
        specialArgs = {inherit inputs outputs;};
        modules = [./hosts/paathshala/configuration.nix];
      };

      lle = nixpkgs-stable.lib.nixosSystem rec {
        system = x86linux;
        specialArgs = {inherit inputs outputs;};
        modules = [./hosts/lle/configuration.nix];
      };
    };

    # First time: nix run nix-darwin -- switch --flake .#mancha
    # darwin-rebuild build --flake .#hostname
    darwinConfigurations = {
      mancha = nix-darwin.lib.darwinSystem rec {
        specialArgs = {inherit inputs outputs;};
        modules = [./hosts/mancha/configuration.nix];
      };
    };

    # First time : nix run home-manager/release-25.11 -- switch --flake .#username@hostname
    # Then after : home-manager switch --flake .#username@hostname
    homeConfigurations = {
      "suvash@paathshala" = home-manager.lib.homeManagerConfiguration {
        pkgs =
          nixpkgs-stable.legacyPackages.${x86linux}; # required by home-manager
        extraSpecialArgs = {inherit inputs outputs;};
        modules = [./hosts/paathshala/home-manager.nix];
      };
    };
  };
}
