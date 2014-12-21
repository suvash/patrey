set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8

fish_user_abbreviations

if test -f ~/.computer
  . ~/.computer
end
