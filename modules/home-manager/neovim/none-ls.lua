-- plugin originally called null-ls, hence the naming
local null_ls = require("null-ls")

null_ls.setup({
  sources = {
    -- nix
    null_ls.builtins.formatting.alejandra,
    null_ls.builtins.diagnostics.deadnix,
    null_ls.builtins.code_actions.statix,
    -- sh/bash
    null_ls.builtins.formatting.beautysh,
    null_ls.builtins.diagnostics.shellcheck,
    null_ls.builtins.code_actions.shellcheck,
    null_ls.builtins.hover.printenv,
    -- git
    null_ls.builtins.code_actions.gitsigns,
    -- spellcheck
    null_ls.builtins.completion.spell,
  },
})
