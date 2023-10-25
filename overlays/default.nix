{inputs, ...}: {
  # This one brings our custom packages from the 'pkgs' directory
  additions = final: _prev: import ../pkgs {pkgs = final;};

  # This one contains whatever you want to overlay
  # You can change versions, add patches, set compilation flags, anything really.
  # https://nixos.wiki/wiki/Overlays
  modifications = final: prev: {
    # example = prev.example.overrideAttrs (oldAttrs: rec {
    # ...
    # });
  };

  # When applied, the nixpkgs set (declared in the flake inputs) will
  # be accessible through 'pkgs.unstable', 'pkgs.master' & so on
  unstable-packages = final: _prev: {
    unstable = import inputs.nixpkgs-unstable {
      system = final.system;
      config.allowUnfree = true;
    };
  };

  master-packages = final: _prev: {
    master = import inputs.nixpkgs-master {
      system = final.system;
      config.allowUnfree = true;
    };
  };

  sha-3be4a51-packages = final: _prev: {
    sha-3be4a51 = import inputs.nixpkgs-3be4a51 {
      system = final.system;
      config.allowUnfree = true;
    };
  };
}
