-- KEYMAPS

-- leader needs some space
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Normal

-- Explore
vim.keymap.set("n", "<leader>..", ":Ex<CR>")
vim.keymap.set("n", "<leader>n.", ":NvimTreeFindFileToggle<CR>")

-- Trouble
vim.keymap.set("n", "<leader>tt", ":TroubleToggle<CR>")


-- Resize with arrows
vim.keymap.set("n", "<C-Up>", ":resize +5<CR>")
vim.keymap.set("n", "<C-Down>", ":resize -5<CR>")
vim.keymap.set("n", "<C-Left>", ":vertical resize -5<CR>")
vim.keymap.set("n", "<C-Right>", ":vertical resize +5<CR>")

-- previous next delete buffer
vim.keymap.set("n", "<leader>bp", vim.cmd.bp)
vim.keymap.set("n", "<leader>bn", vim.cmd.bn)
vim.keymap.set("n", "<leader>bs", vim.cmd.w)
vim.keymap.set("n", "<leader>bd", vim.cmd.Bd) -- not the usual bd, Bd from bbye
vim.keymap.set("n", "<leader>bx", vim.cmd.bd) -- the usual bd

-- quit
vim.keymap.set("n", "<leader>wq", vim.cmd.wq)
vim.keymap.set("n", "<leader>qa", vim.cmd.qa)
vim.keymap.set("n", "<leader>qa", vim.cmd.qa)

-- Insert
