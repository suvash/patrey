set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8
set -x EDITOR vim
set -x TERM xterm-256color

switch (uname)
case Darwin
  set -x GPG_TTY (tty)
  set -x PATH /usr/local/bin $PATH
  gpg-connect-agent /bye
  set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
end

# Load fish abbreviations in interactive env
if status --is-interactive
    fish_user_abbreviations
end

# Keep it around in case something goes wrong
# gpg-connect-agent updatestartuptty /bye > /dev/null

# Add local bin to PATH
if test -d $HOME/.local/bin
  set -x PATH $HOME/.local/bin $PATH
end

# Add $HOME/.cargo/bin to PATH if exists
if test -d $HOME/.cargo/bin
  set -x PATH $HOME/.cargo/bin $PATH
end

# Source $HOME/.computer if exists
if test -f $HOME/.computer
  source $HOME/.computer
end

# Conda in path if exists
switch (uname)
case Darwin
  if test -d /usr/local/miniconda3
    set -x PATH /usr/local/miniconda3/bin $PATH
  end
end


# Start tmux if exists in a alacritty session
if type -q tmux; and set -q ALACRITTY; and not set -q TMUX
    exec tmux
end
