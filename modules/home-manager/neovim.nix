{
  inputs,
  pkgs,
  ...
}: let
  lsp-zero-nvim-3 = pkgs.vimUtils.buildVimPlugin {
    name = "lsp-zero-nvim-3";
    src = inputs.lsp-zero-nvim-3;
  };
in {
  home.packages = with pkgs; [
    fzy # telescope fzy extension

    # tools used by lsp via none-ls
    # nix
    unstable.alejandra
    deadnix
    statix
    # bash sh
    shfmt
    shellharden
    # linters
    codespell
    gitlint

    # language servers these should really be set up in individual projects
    # lua
    luaPackages.luacheck
    lua-language-server
    # vscode extracted lang servers - json, html
    nodePackages.vscode-langservers-extracted
    # nodejs - js - ts
    nodePackages.typescript
    nodePackages.typescript-language-server
    # python
    pyright
    ruff
    # elixir
    elixir-ls
    # css
    tailwindcss-language-server
  ];

  programs.neovim = {
    enable = true;

    withNodeJs = true;
    withPython3 = true;
    withRuby = true;

    extraLuaConfig = builtins.readFile ./neovim/options.lua + builtins.readFile ./neovim/keymaps.lua;

    plugins = with pkgs;
      [
        # Notify
        {
          plugin = vimPlugins.nvim-notify;
          type = "lua";
          config = ''
            vim.notify = require("notify")
          '';
        }

        # Indent
        {
          plugin = unstable.vimPlugins.indent-blankline-nvim;
          type = "lua";
          config = ''
            require('ibl').setup()
          '';
        }

        # Dired
        {
          plugin = unstable.vimPlugins.indent-blankline-nvim;
          type = "lua";
          config = ''
            require('ibl').setup()
          '';
        }

        # Leap
        {
          plugin = vimPlugins.leap-nvim;
          type = "lua";
          config = ''
            require('leap').add_default_mappings()
          '';
        }

        {
          plugin = vimPlugins.which-key-nvim;
          type = "lua";
          config = ''
            require('which-key').setup()
          '';
        }

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

        vimPlugins.vim-illuminate

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

        {
          plugin = vimPlugins.nvim-ts-autotag;
          type = "lua";
          config = ''
            require("nvim-ts-autotag").setup()
          '';
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
      ]
      ++ [
        # LSP

        # those without dedicated lsp (or other tools)
        {
          plugin = unstable.vimPlugins.none-ls-nvim;
          type = "lua";
          config = builtins.readFile ./neovim/none-ls.lua;
        }

        {
          plugin = pkgs.vimPlugins.nvim-lspconfig;
          type = "lua";
          config = ''
            require'lspconfig'.lua_ls.setup({})

            require'lspconfig'.jsonls.setup({})
            require'lspconfig'.tsserver.setup({})

            require'lspconfig'.pyright.setup({})
            require'lspconfig'.ruff.setup({})

            require'lspconfig'.elixirls.setup({
              cmd = { "${pkgs.elixir-ls}/lib/language_server.sh" };
            })

            require'lspconfig'.tailwindcss.setup({
              init_options = {
                userLanguages = {
                  elixir = "html-eex",
                  eelixir = "html-eex",
                  heex = "html-eex",
                },
              },
            })

            --Enable (broadcasting) snippet capability for completion
            local snippet_capabilities = vim.lsp.protocol.make_client_capabilities()
            snippet_capabilities.textDocument.completion.completionItem.snippetSupport = true

            require'lspconfig'.html.setup {
              capabilities = snippet_capabilities,
              configurationSection = { "html", "css", "javascript" },
              embeddedLanguages = {
                css = true,
                javascript = true
              },
              provideFormatter = true
            }
          '';
        }

        {
          plugin = lsp-zero-nvim-3;
          type = "lua";
          config = builtins.readFile ./neovim/lsp-zero-nvim.lua;
        }

        {
          plugin = pkgs.unstable.vimPlugins.nvim-lsp-notify;
          type = "lua";
          config = ''
            require("lsp-notify").setup()
          '';
        }
      ];
  };
}
