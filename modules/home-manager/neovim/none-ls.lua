-- plugin originally called null-ls, hence the naming
local null_ls = require("null-ls")

null_ls.setup({
  sources = {
    -- nix
    null_ls.builtins.formatting.alejandra,
    null_ls.builtins.diagnostics.deadnix,
    null_ls.builtins.code_actions.statix,
    -- sh/bash
    null_ls.builtins.formatting.shellharden,
    null_ls.builtins.formatting.shfmt,
    null_ls.builtins.hover.printenv,
    -- git
    null_ls.builtins.code_actions.gitsigns,
    null_ls.builtins.diagnostics.gitlint,
    -- spellcheck
    null_ls.builtins.completion.spell,
    null_ls.builtins.diagnostics.codespell,
  },
})
