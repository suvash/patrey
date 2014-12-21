function nix-search
	nix-env -qaP '*' --description | grep $argv
end
