set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8

fish_user_abbreviations

switch (uname)
  case Darwin
    set PATH $HOME/.nix-profile/bin $PATH
    set -x NIX_PATH $HOME/Developer/nixpkgs:nixpkgs=$HOME/Developer/nixpkgs
  case Linux
    echo "On a Linux."
  case '*'
    echo 'Whoa ! what os is this ?'
end

if test -f ~/.computer
  . ~/.computer
end
