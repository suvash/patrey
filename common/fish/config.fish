set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8
set -x EDITOR vim
set -x TERM xterm-256color
set -x GPG_TTY (tty)

gpg-connect-agent /bye
gpg-connect-agent updatestartuptty /bye > /dev/null
set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)

fish_user_abbreviations

if test -f ~/.computer
  . ~/.computer
end