set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8
set -x EDITOR vim
set -x TERM xterm-256color

fish_user_abbreviations

switch (uname)
  case Darwin
  case Linux
  case '*'
    echo 'Whoa ! what os is this ?'
end

switch (hostname)
  case yantra
    export (gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
  case '*'
end

if test -f ~/.computer
  . ~/.computer
end

if test -f ~/.nix-profile/etc/profile.d/nix.sh
 sh ~/.nix-profile/etc/profile.d/nix.sh
end
