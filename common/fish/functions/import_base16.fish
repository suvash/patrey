function import_base16
  if not test -d $HOME/.config/base16-shell
    echo "Dowloading base16-shell..."
    git clone https://github.com/chriskempson/base16-shell.git $HOME/.config/base16-shell
  end

  source "$HOME/.config/base16-shell/profile_helper.fish"
end
