local lspconfig = require('lspconfig')

lspconfig.lua_ls.setup({})

lspconfig.jsonls.setup({})
lspconfig.tsserver.setup({})

--Enable (broadcasting) snippet capability for completion
local snippet_capabilities = vim.lsp.protocol.make_client_capabilities()
snippet_capabilities.textDocument.completion.completionItem.snippetSupport = true

require 'lspconfig'.html.setup {
  capabilities = snippet_capabilities,
  configurationSection = { "html", "css", "javascript" },
  embeddedLanguages = {
    css = true,
    javascript = true
  },
  provideFormatter = true
}
