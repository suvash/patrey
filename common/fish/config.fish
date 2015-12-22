set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8
set -x EDITOR vim
set -x TERM xterm-256color

fish_user_abbreviations

switch (uname)
  case Darwin
    set PATH $HOME/.nix-profile/bin /usr/local/sbin $PATH
    set -x NIX_PATH $HOME/Developer/nixpkgs:nixpkgs=$HOME/Developer/nixpkgs
  case Linux
  case '*'
    echo 'Whoa ! what os is this ?'
end

if test -f ~/.computer
  . ~/.computer
end

if test -f ~/.nix-profile/etc/profile.d/nix.sh
 sh ~/.nix-profile/etc/profile.d/nix.sh
end
