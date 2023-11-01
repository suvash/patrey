{
  pkgs,
  config,
  ...
}: {
  home.packages = with pkgs; [
    fzy

    # language servers
    elixir-ls
    python311Packages.ruff-lsp
    nodePackages.bash-language-server
    nodePackages.dockerfile-language-server-nodejs
    nodePackages.typescript-language-server
    rust-analyzer
    terraform-ls
  ];

  programs.neovim = {
    enable = true;

    withNodeJs = false;
    withPython3 = false;
    withRuby = false;

    extraLuaConfig = builtins.readFile ./neovim/sanity.lua + builtins.readFile ./neovim/mappings.lua + "\n--PLUGINS\n";

    plugins = with pkgs.vimPlugins; [
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
        plugin = gitsigns-nvim;
        type = "lua";
        config = ''
          require('gitsigns').setup()
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

      {
        plugin = nvim-lspconfig;
        type = "lua";
        config = builtins.readFile ./neovim/nvim-lspconfig.lua;
      }

      # autocompletion
      nvim-cmp
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp_luasnip
      cmp-nvim-lua
    ];
  };
}
