{ inputs, pkgs, ... }:
let
  lsp-zero-nvim-3 = pkgs.vimUtils.buildVimPlugin {
    name = "lsp-zero-nvim-3";
    src = inputs.lsp-zero-nvim-3;
  };
in {
  home.packages = with pkgs; [
    fzy # telescope fzy extension

    # diagnostics tools used by lsp via none-ls
    # bash
    shfmt
    shellcheck
    # nix
    unstable.alejandra
    # js
    nodePackages.eslint
    # js
    stylua
    luaPackages.luacheck
    # linters

    # language servers these should really be set up in individual projects
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

    extraLuaConfig = builtins.readFile ./neovim/options.lua
      + builtins.readFile ./neovim/keymaps.lua + ''

        --PLUGINS
      '';

    plugins = with pkgs;
      [
        {
          plugin = vimPlugins.vim-bbye;
          type = "lua";
        }

        {
          plugin = vimPlugins.telescope-nvim;
          type = "lua";
          config = builtins.readFile ./neovim/telescope-nvim.lua;
        }

        {
          plugin = vimPlugins.telescope-fzy-native-nvim;
          type = "lua";
          config = ''
            require("telescope").load_extension("fzy_native")
          '';
        }

        {
          plugin = vimPlugins.nvim-web-devicons;
          type = "lua";
          config = builtins.readFile ./neovim/nvim-web-devicons.lua;
        }

        {
          plugin = vimPlugins.nvim-treesitter.withAllGrammars;
          type = "lua";
          config = builtins.readFile ./neovim/nvim-treesitter.lua;
        }

        {
          plugin = vimPlugins.base16-vim;
          type = "lua";
          config = builtins.readFile ./neovim/base16-vim.lua;
        }

        {
          plugin = vimPlugins.undotree;
          type = "lua";
          config = ''
            vim.keymap.set("n", "<leader>ut", vim.cmd.UndotreeToggle)
          '';
        }

        {
          plugin = vimPlugins.vim-fugitive;
          type = "lua";
          config = ''
            vim.keymap.set("n", "<leader>gg", vim.cmd.Git)
          '';
        }

        {
          plugin = vimPlugins.comment-nvim;
          type = "lua";
          config = ''
            require("Comment").setup()
          '';
        }

        {
          plugin = vimPlugins.nvim-ts-autotag;
          type = "lua";
          config = ''
            require("nvim-ts-autotag").setup()
          '';
        }

        {
          plugin = vimPlugins.bufferline-nvim;
          type = "lua";
          config = ''
            require("bufferline").setup()
          '';
        }

        {
          plugin = vimPlugins.gitsigns-nvim;
          type = "lua";
          config = ''
            require("gitsigns").setup()
          '';
        }

        {
          plugin = vimPlugins.nvim-tree-lua;
          type = "lua";
          config = ''
            require("nvim-tree").setup()
          '';
        }

        {
          plugin = vimPlugins.nvim-autopairs;
          type = "lua";
          config = ''
            require("nvim-autopairs").setup()
          '';
        }

        # Formatting
        {
          plugin = unstable.vimPlugins.none-ls-nvim;
          type = "lua";
          config = builtins.readFile ./neovim/none-ls.lua;
        }

        # CMP

        vimPlugins.friendly-snippets

        {
          plugin = vimPlugins.luasnip;
          type = "lua";
          config = ''
            require("luasnip.loaders.from_vscode").lazy_load()
          '';
        }

        vimPlugins.lspkind-nvim # cmp formatting
        vimPlugins.cmp-cmdline # cmdline completion source
        vimPlugins.cmp-calc # calc completion source
        vimPlugins.cmp-buffer # buffer completion source
        vimPlugins.cmp-path # path completion source
        vimPlugins.cmp-nvim-lsp # lsp completion source
        vimPlugins.cmp_luasnip # luasnip completion source
        vimPlugins.cmp-nvim-lua # nvim lua runtim api source

        {
          plugin = vimPlugins.nvim-cmp;
          type = "lua";
          config = builtins.readFile ./neovim/nvim-cmp.lua;
        }

        # Trouble
        {
          plugin = vimPlugins.trouble-nvim;
          type = "lua";
          config = ''
            require("trouble").setup()
          '';
        }

        # Terminal
        {
          plugin = vimPlugins.toggleterm-nvim;
          type = "lua";
          config = builtins.readFile ./neovim/toggleterm-nvim.lua;
        }
      ] ++ [
        # LSP

        pkgs.vimPlugins.nvim-lspconfig

        {
          plugin = lsp-zero-nvim-3;
          type = "lua";
          config = builtins.readFile ./neovim/lsp-zero-nvim.lua;
        }
      ];
  };
}
