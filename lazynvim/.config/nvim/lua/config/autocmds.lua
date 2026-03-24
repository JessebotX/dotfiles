-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

vim.api.nvim_create_autocmd("FileType", {
    pattern = "css",
    callback = function()
        vim.bo.tabstop = 1
        vim.bo.shiftwidth = 1
        vim.bo.softtabstop = 1
        vim.bo.expandtab = true
    end,
})

vim.api.nvim_create_autocmd("FileType", {
    pattern = "html",
    callback = function()
        vim.bo.tabstop = 1
        vim.bo.shiftwidth = 1
        vim.bo.softtabstop = 1
        vim.bo.expandtab = true
    end,
})

vim.api.nvim_create_autocmd("FileType", {
    pattern = "go",
    callback = function()
        vim.bo.tabstop = 4
        vim.bo.shiftwidth = 4
        vim.bo.softtabstop = 4
        vim.bo.expandtab = false
    end,
})

vim.api.nvim_create_autocmd("FileType", {
    pattern = "markdown",
    callback = function()
        vim.bo.tabstop = 2
        vim.bo.shiftwidth = 2
        vim.bo.softtabstop = 2
        vim.bo.expandtab = true
        vim.bo.textwidth = 70
        vim.bo.wrapmargin = 70
    end,
})

vim.api.nvim_create_autocmd("FileType", {
    pattern = "make",
    callback = function()
        vim.bo.tabstop = 4
        vim.bo.shiftwidth = 4
        vim.bo.softtabstop = 4
        vim.bo.expandtab = false
    end,
})
