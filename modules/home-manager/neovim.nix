{
  pkgs,
  config,
  ...
}: {
  home.packages = with pkgs; [
    fzy

    # language servers
    elixir-ls
    lua-language-server
    python311Packages.ruff-lsp
    nodePackages.bash-language-server
    nodePackages.dockerfile-language-server-nodejs
    nodePackages.typescript-language-server
    rust-analyzer
    terraform-ls
  ];

  programs.neovim = {
    enable = true;

    withNodeJs = true;
    withPython3 = true;
    withRuby = true;

    extraLuaConfig = builtins.readFile ./neovim/options.lua + builtins.readFile ./neovim/keymaps.lua + "\n--PLUGINS\n";

    plugins = with pkgs.vimPlugins; [
      {
        plugin = dashboard-nvim;
        type = "lua";
        config = builtins.readFile ./neovim/dashboard-nvim.lua;
      }

      {
        plugin = vim-bbye;
        type = "lua";
      }

      {
        plugin = telescope-nvim;
        type = "lua";
        config = builtins.readFile ./neovim/telescope-nvim.lua;
      }

      {
        plugin = telescope-fzy-native-nvim;
        type = "lua";
        config = ''
          require('telescope').load_extension('fzy_native')
        '';
      }

      {
        plugin = nvim-web-devicons;
        type = "lua";
        config = builtins.readFile ./neovim/nvim-web-devicons.lua;
      }

      {
        plugin = nvim-treesitter.withAllGrammars;
        type = "lua";
        config = builtins.readFile ./neovim/nvim-treesitter.lua;
      }

      {
        plugin = base16-vim;
        type = "lua";
        config = builtins.readFile ./neovim/base16-vim.lua;
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
        plugin = nvim-lspconfig;
        type = "lua";
        config = builtins.readFile ./neovim/nvim-lspconfig.lua;
      }

      {
        plugin = nvim-ts-autotag;
        type = "lua";
        config = ''
          require('nvim-ts-autotag').setup()
        '';
      }

      {
        plugin = bufferline-nvim;
        type = "lua";
        config = ''
          require("bufferline").setup{}
        '';
      }

      {
        plugin = gitsigns-nvim;
        type = "lua";
        config = ''
          require('gitsigns').setup()
        '';
      }

      {
        plugin = lspkind-nvim;
        type = "lua";
        config = builtins.readFile ./neovim/lspkind-nvim.lua;
      }

      # other
      nvim-autopairs

      # dependency for luasnip
      friendly-snippets

      # snippets
      {
        plugin = luasnip;
        type = "lua";
        config = ''
          require("luasnip.loaders.from_vscode").lazy_load()
        '';
      }

      # completions
      {
        plugin = nvim-cmp;
        type = "lua";
        config = builtins.readFile ./neovim/nvim-cmp.lua;
      }

      cmp-buffer # buffer completion source
      cmp-path # path completion source
      cmp-cmdline # cmdline completion source
      cmp-nvim-lsp # lsp completion source
      cmp_luasnip # luasnip completion source
      cmp-nvim-lua # nvim lua runtim api source

    ];
  };
}
