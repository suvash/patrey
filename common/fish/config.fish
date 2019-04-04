set -x LANG en_US.UTF-8
set -x LC_ALL en_US.UTF-8
set -x EDITOR vim

switch (uname)
case Darwin
  set -x GPG_TTY (tty)
  gpg-connect-agent --quiet updatestartuptty /bye > /dev/null
  set -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
end

# Sway
if test -n "$WAYLAND_DISPLAY"
  set -x GPG_TTY (tty)
  gpg-connect-agent --quiet updatestartuptty /bye > /dev/null
end

# Load fish abbreviations in interactive env
if status --is-interactive
    fish_user_abbreviations
end

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
    source /usr/local/miniconda3/etc/fish/conf.d/conda.fish
  end
end
