-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

vim.keymap.set("n", "j", 'v:count == 0 ? "gj" : "j"', { expr = true, silent = true })
vim.keymap.set("v", "j", 'v:count == 0 ? "gj" : "j"', { expr = true, silent = true })

vim.keymap.set("n", "k", 'v:count == 0 ? "gk" : "k"', { expr = true, silent = true })
vim.keymap.set("v", "k", 'v:count == 0 ? "gk" : "k"', { expr = true, silent = true })

vim.keymap.set("n", "0", 'v:count == 0 ? "g0" : "0"', { expr = true, silent = true })
vim.keymap.set("v", "0", 'v:count == 0 ? "g0" : "0"', { expr = true, silent = true })

vim.keymap.set("n", "$", 'v:count == 0 ? "g$" : "$"', { expr = true, silent = true })
vim.keymap.set("v", "$", 'v:count == 0 ? "g$" : "$"', { expr = true, silent = true })
