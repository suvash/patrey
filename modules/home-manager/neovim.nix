{
  pkgs,
  config,
  ...
}: {
  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      {
        plugin = telescope-nvim;
        type = "lua";
        config = ''
          local builtin = require('telescope.builtin')
          vim.keymap.set('n', '<leader>pf', builtin.find_files, {})
          vim.keymap.set('n', '<leader>pg', builtin.live_grep, {})
        '';
      }

      {
        plugin = nvim-treesitter.withAllGrammars;
        type = "lua";
        config = ''
          require'nvim-treesitter.configs'.setup {
            highlight = {
              -- `false` will disable the whole extension
              enable = true,
              -- list of parsers that will be disabled
              disable = { },
              --
              additional_vim_regex_highlighting = false,
            },
          }
        '';
      }

      {
        plugin = base16-vim;
        type = "lua";
        config = ''
          local current_theme_name = os.getenv('BASE16_THEME')
          if current_theme_name and vim.g.colors_name ~= 'base16-'..current_theme_name then
            vim.cmd('let base16colorspace=256')
            vim.cmd('colorscheme base16-'..current_theme_name)
          end
        '';
      }

      {
        plugin = undotree;
        type = "lua";
        config = ''
          vim.keymap.set('n', '<leader>ut', vim.cmd.UndotreeToggle)
        '';
      }

      {
        plugin = vim-fugitive;
        type = "lua";
        config = ''
          vim.keymap.set('n', '<leader>gg', vim.cmd.Git)
        '';
      }

      {
        plugin = comment-nvim;
        type = "lua";
        config = ''
          require('Comment').setup()
        '';
      }

      {
        plugin = luasnip;
        type = "lua";
        config = ''
          require("luasnip.loaders.from_vscode").lazy_load()
        '';
      }

      {
        # dependency for luasnip
        plugin = friendly-snippets;
      }

      # autocompletion
      nvim-cmp
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp_luasnip
      cmp-nvim-lua
    ];

    extraLuaConfig = ''
      -- SANITY
      vim.g.mapleader = " "

      vim.opt.number = true;
      vim.opt.relativenumber = true;

      vim.opt.tabstop = 2
      vim.opt.softtabstop = 2
      vim.opt.shiftwidth = 2
      vim.opt.expandtab = true

      vim.opt.smartindent = true
      vim.opt.cursorline = true

      vim.opt.wrap = true

      vim.opt.swapfile = false
      vim.opt.backup = false
      vim.opt.undodir = "${config.xdg.stateHome}/nvim/undodir"
      vim.opt.undofile = true

      vim.opt.hlsearch = false
      vim.opt.incsearch = true

      vim.opt.termguicolors = true

      vim.opt.scrolloff = 8
      vim.opt.signcolumn = "yes"
      vim.opt.isfname:append("@-@")

      vim.opt.updatetime = 50

      vim.opt.colorcolumn = "80"

      -- MAPPING

      -- netrw
      vim.keymap.set("n", "<leader>o-", vim.cmd.Ex)

      -- previous next buffer
      vim.keymap.set("n", "<leader>bp", vim.cmd.bp)
      vim.keymap.set("n", "<leader>bn", vim.cmd.bn)

      -- PLUGINS CONFIG
    '';
  };
}
