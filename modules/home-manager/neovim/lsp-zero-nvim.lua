local lsp_zero = require("lsp-zero")

lsp_zero.on_attach(function(client, bufnr)
	-- see :help lsp-zero-keybindings
	-- to learn the available actions
	lsp_zero.default_keymaps({ buffer = bufnr })
end)

local lspconfig = require("lspconfig")

lspconfig.bashls.setup({})
lspconfig.dockerls.setup({})
lspconfig.elixirls.setup({}) -- configure the cmd
-- lspconfig.emmet_ls.setup({}) -- install language server
lspconfig.lua_ls.setup({}) -- configure the cmd
lspconfig.ruff_lsp.setup({})
lspconfig.rust_analyzer.setup({})
-- lspconfig.tailwindcss.setup({}) -- install language server
lspconfig.terraformls.setup({})
lspconfig.tsserver.setup({})
