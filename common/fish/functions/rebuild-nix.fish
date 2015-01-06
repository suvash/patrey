function rebuild-nix
  switch (uname)
    case Darwin
      nix-env -iA nixpkgs.all
    case Linux
      sudo nixos-rebuild switch
  end
end
