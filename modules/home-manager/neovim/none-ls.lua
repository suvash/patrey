-- plugin originally called null-ls, hence the naming
local null_ls = require("null-ls")

null_ls.setup({
  sources = {
    -- nix
    null_ls.builtins.formatting.alejandra,
    null_ls.builtins.diagnostics.deadnix,
    null_ls.builtins.code_actions.statix,
    -- git
    null_ls.builtins.code_actions.gitsigns,
    -- sh/bash
    null_ls.builtins.formatting.shfmt,
    -- spellcheck
    null_ls.builtins.completion.spell,
  },
})
