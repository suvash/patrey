{pkgs, ...}: {
  home.packages = with pkgs; [
    tmux-mem-cpu-load
  ];
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
      # escape time (for vim)
      set -sg escape-time 1

      # bind r to source tmux conf
      bind C-r source-file ~/.config/tmux/tmux.conf \; display 'Sourced ~/.config/tmux/tmux.conf'

      # Automatically set window title
      set-window-option -g automatic-rename on
      set -g set-titles on

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

      # new window
      bind C-c new-window

      # Status
      set -g status-position top
      set -g status on
      set -g status-justify "centre"
      set -g status-interval 4
      set -g status-left-length 28
      set -g status-left "#(hostname | cut --characters 1-4).. | #(tmux-mem-cpu-load --interval 4 --averages-count 0 --graph-lines 0)"
      set -g status-right-length 28
      set -g status-right "#(date +'%H:%M | %a %Y/%m/%d | W%V')"

      # Show hide status Bar
      bind C-s set -g status
    '';
  };
}
