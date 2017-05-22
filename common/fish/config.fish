set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8
set -x EDITOR vim
set -x TERM xterm-256color
set -x GPG_TTY (tty)

fish_user_abbreviations

if test -f ~/.computer
  . ~/.computer
end