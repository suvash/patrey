local lsp_zero = require("lsp-zero")

lsp_zero.on_attach(function(client, bufnr)
    local opts = { buffer = bufnr }
    lsp_zero.default_keymaps(opts)

    vim.keymap.set({ 'n', 'x' }, '<leader>fb', function()
        vim.lsp.buf.format({ async = false, timeout_ms = 10000 })
    end, opts)
end)

lsp_zero.set_sign_icons({
    error = '✘',
    warn = '▲',
    hint = '⚑',
    info = '»'
})

lsp_zero.format_on_save({
    format_opts = {
        async = false,
        timeout_ms = 10000,
    },
    servers = {
        ['null-ls'] = { 'nix', 'sh', 'bash' },
        ['html'] = { 'html' },
        ['lua_ls'] = { 'lua' },
        ['jsonls'] = { 'json' },
        ['ts_ls'] = { 'javascript', 'typescript' },
        ['elixirls'] = { 'elixir', 'heex' },
        ['ruff'] = { 'python' },
    }
})
