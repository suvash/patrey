-- plugin originally called null-ls, hence the naming
local null_ls = require("null-ls")

null_ls.setup({
  sources = {
    -- code actions
    -- null_ls.builtins.code_actions.gitsigns,
    -- formatting
    null_ls.builtins.formatting.shfmt,
    null_ls.builtins.formatting.alejandra,
    null_ls.builtins.formatting.stylua,
    -- diagnostics
    null_ls.builtins.diagnostics.shellcheck,
    null_ls.builtins.diagnostics.luacheck,
    -- completion, should really be using cmp instead
    null_ls.builtins.completion.spell,
  },
})
