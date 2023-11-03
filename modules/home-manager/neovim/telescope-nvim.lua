local telescope = require('telescope')
local telebuiltin = require('telescope.builtin')
local teleactions = require('telescope.actions')

telescope.setup {
  defaults = {
    mappings = {
      i = {
        ["<esc>"] = teleactions.close
      },
    },
  },
  extensions = {
    fzy_native = {
      override_generic_sorter = false,
      override_file_sorter = true,
    }
  }
}


vim.keymap.set('n', '<leader>pf', telebuiltin.find_files, {})
vim.keymap.set('n', '<leader>pg', telebuiltin.live_grep, {})

