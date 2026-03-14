-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

vim.o.tabstop = 4
vim.o.shiftwidth = 0
vim.o.softtabstop = -1
vim.o.expandtab = true

vim.o.guifont = "Maple Mono NL NF:h14"

vim.o.linebreak = true
vim.o.wrap = true
vim.o.completeopt = "noselect"
vim.o.spell = false

vim.opt.listchars = { tab = "| ", trail = "·", nbsp = "␣" }

--vim.g.neovide_scroll_animation_length = 0
vim.g.neovide_scroll_animation_length = 0.15
vim.g.neovide_scroll_animation_far_lines = 1
