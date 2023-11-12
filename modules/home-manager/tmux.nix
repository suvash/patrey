{inputs, ...}: {
  programs.tmux = {
    enable = true;
    shortcut = "a";
    baseIndex = 1;
    clock24 = true;
    historyLimit = 100000;
    keyMode = "emacs";
    terminal = "screen-256color";
    sensibleOnTop = true;
    extraConfig = ''
      # Automatically set window title
      set-window-option -g automatic-rename on
      set-option -g set-titles on

      # Remap split keys
      unbind %
      bind \\ split-window -h
      bind - split-window -v

      # Remap movement keys
      bind h select-pane -L
      bind j select-pane -D
      bind k select-pane -U
      bind l select-pane -R

      # Resize panes
      bind H resize-pane -L 5
      bind J resize-pane -D 5
      bind K resize-pane -U 5
      bind L resize-pane -R 5

      # Set window notifications
      setw -g monitor-activity on
      setw -g visual-activity on

      # Rename a new window properly (default working dir)
      bind c new-window \; command-prompt -p "Name for this new window: " "rename-window '%%'"

      # base16
      source-file ${inputs.base16-tmux}/colors/base16-onedark.conf
    '';
  };
}
