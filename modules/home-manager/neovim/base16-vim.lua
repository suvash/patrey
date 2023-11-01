local current_theme_name = os.getenv('BASE16_THEME')
if current_theme_name and vim.g.colors_name ~= 'base16-'..current_theme_name then
  vim.cmd('let base16colorspace=256')
  vim.cmd('colorscheme base16-'..current_theme_name)
end
