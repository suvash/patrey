-- OPTIONS

-- :help options
vim.opt.fileencoding = "utf-8"                                      -- all your utf-8 are belong to us
vim.opt.swapfile = false                                            -- no swap file
vim.opt.backup = false                                              -- no backup file
vim.opt.undofile = true                                             -- enable persistent undo
vim.opt.undodir = os.getenv("HOME") .. "/.local/state/nvim/undodir" -- undo dir path

vim.opt.clipboard = "unnamedplus"                                   -- access the system clipboard

vim.opt.cursorline = true                                           -- highlight the current line
vim.opt.number = true                                               -- set numbered lines
vim.opt.relativenumber = true                                       -- set relative numbered lines
vim.opt.numberwidth = 4                                             -- set number column width to 2 {default 4}
vim.opt.signcolumn =
"yes"                                                               -- always show the sign column, otherwise it would shift the text each time

vim.opt.splitbelow = true                                           -- all horizontal splits to go below current window
vim.opt.splitright = true                                           -- all vertical splits to go to the right of current window

vim.opt.termguicolors = false                                       -- disable true color support

vim.opt.scrolloff = 8                                               -- minimum number of rows to start scrolling at the edges
vim.opt.sidescrolloff = 8                                           -- minimum number of columns to start scrolling at the edges

vim.opt.expandtab = true                                            -- convert tabs to spaces
vim.opt.shiftwidth = 2                                              -- the number of spaces inserted for each indentation
vim.opt.tabstop = 2                                                 -- insert 2 spaces for a tab
vim.opt.showtabline = 2                                             -- always show tabs
vim.opt.wrap = false                                                -- display lines as one long line

vim.opt.smartcase = true                                            -- use smart case
vim.opt.smartindent = true                                          -- use smart indentation

vim.opt.hlsearch = false                                            -- highlight all matches on previous search pattern
vim.opt.incsearch = true                                            -- incremental search much better

vim.opt.timeout = true;                                             -- turn on timeout
vim.opt.timeoutlen = 500                                            -- time to wait for a mapped sequence to complete (in milliseconds)
vim.opt.updatetime = 300                                            -- faster completion (4000ms default)

vim.opt.colorcolumn = "120"                                         -- color the col at 120 chars

vim.opt.ignorecase = true                                           -- ignore case in search patterns
vim.opt.mouse = "a"                                                 -- allow the mouse to be used in neovim in all modes
vim.opt.pumheight = 10                                              -- pop up menu max. height

vim.opt.cmdheight = 1                                               -- more space in the neovim command line for displaying messages
vim.opt.completeopt = { "menuone", "noselect" }                     -- later to be used by completion plugins

vim.opt.list = true                                                 -- list special characters
vim.opt.listchars = {
  tab = "»·",
  nbsp = "+",
  trail = '.',
  extends = "→",
  precedes = "←"
}
