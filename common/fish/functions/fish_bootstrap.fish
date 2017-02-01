function fish_bootstrap

  # Disable greeting
  set -U fish_greeting

  set -Ux EDITOR vim

  #Prompt colors
  set -U fish_color_cwd green
  set -U fish_color_host magenta
  set -U fish_color_user cyan
  set -U fish_pager_color_description 555 yellow


end
