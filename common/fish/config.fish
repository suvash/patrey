set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8
set -x EDITOR vim
set -x TERM xterm-256color

switch (uname)
case Darwin
  set -x GPG_TTY (tty)
  gpg-connect-agent /bye
  set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
end

# Keep it around in case something goes wrong
# gpg-connect-agent updatestartuptty /bye > /dev/null

fish_user_abbreviations

if test -f ~/.computer
  . ~/.computer
end