return {
    {
        "nvim-treesitter/nvim-treesitter",
        opts = {
            ensure_installed = {
                "bash",
                "c",
                "cmake",
                "cpp",
                "css",
                "go",
                "html",
                "javascript",
                "json",
                "lua",
                "make",
                "markdown",
                "markdown_inline",
                "ninja",
                "odin",
                "python",
                "query",
                "regex",
                "toml",
                "vim",
                "xml",
                "yaml",
                "zig",
            },
        },
    },
    { "saghen/blink.cmp", enabled = false },
    { "neovim/nvim-lspconfig", enabled = false },
    { "nvim/mini.pairs", enabled = false },
    {
        "hrsh7th/nvim-cmp",
        enabled = false,
        opts = {
            completion = {
                autocomplete = false,
            },
        },
    },
    { "rose-pine/neovim", name = "rose-pine" },
    {
        "LazyVim/LazyVim",
        opts = {
            colorscheme = "rose-pine-dawn",
        },
    },

    {
        "junegunn/goyo.vim",
        keys = {
            { "<leader>tw", ":Goyo<CR>", desc = "Toggle Goyo" },
        },
        config = function()
            vim.api.nvim_create_autocmd("User", {
                pattern = "GoyoEnter",
                callback = function()
                    vim.o.showmode = false
                    vim.o.showcmd = false
                    vim.o.scrolloff = 999
                    vim.o.cursorline = false

                    require("lualine").hide()
                end,
            })
            vim.api.nvim_create_autocmd("User", {
                pattern = "GoyoLeave",
                callback = function()
                    vim.o.showmode = true
                    vim.o.showcmd = true
                    vim.o.scrolloff = 3
                    vim.o.cursorline = false

                    require("lualine").hide({ unhide = true })
                end,
            })
        end,
    },

    { "rafamadriz/friendly-snippets", enabled = false },

    {
        "folke/snacks.nvim",
        opts = {
            indent = {
                enabled = false,
            },
        },
        config = function()
            vim.g.snacks_animate = false
        end,
    },

    { "lewis6991/gitsigns.nvim", enabled = false },
}
