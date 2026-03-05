-- [[ Neovide GUI ]]
if vim.g.neovide then
    -- neovide specific options...
end

vim.opt.guifont = "Maple Mono NL NF:h14"

-- [[ Leader key ]]
-- See `:help mapleader`
--  NOTE: Must happen before plugins are loaded (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- [[ Options ]]

vim.schedule(function()
    vim.o.clipboard = 'unnamedplus'
end)

vim.o.guicursor = ''
vim.o.mouse = 'a'
vim.o.signcolumn = 'yes'

vim.o.showmode = false

vim.o.number = true
vim.o.relativenumber = true
vim.o.scrolloff = 5

vim.o.breakindent = true
vim.o.tabstop = 4
vim.o.shiftwidth = 0
vim.o.softtabstop = -1
vim.o.expandtab = true
vim.o.smartindent = true

vim.o.backup = false
vim.o.swapfile = false
vim.o.undofile = true

vim.o.updatetime = 50
vim.o.timeoutlen = 300

vim.o.confirm = true
vim.o.ignorecase = true
vim.o.inccommand = 'split'
vim.o.smartcase = true

vim.o.splitright = true
vim.o.splitbelow = true

vim.o.list = true
vim.opt.listchars = { tab = '| ', trail = '·', nbsp = '␣' }
vim.opt.fillchars = { eob = ' ' }

vim.o.wrap = true
vim.o.linebreak = true

-- [[ Keymaps ]]

vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

vim.keymap.set('n', '<C-h>', '<C-w><C-h>', { desc = 'Move focus to the left window' })
vim.keymap.set('n', '<C-l>', '<C-w><C-l>', { desc = 'Move focus to the right window' })
vim.keymap.set('n', '<C-j>', '<C-w><C-j>', { desc = 'Move focus to the lower window' })
vim.keymap.set('n', '<C-k>', '<C-w><C-k>', { desc = 'Move focus to the upper window' })

vim.keymap.set('n', 'j', 'v:count == 0 ? "gj" : "j"', { expr = true, silent = true })
vim.keymap.set('n', 'k', 'v:count == 0 ? "gk" : "k"', { expr = true, silent = true })

vim.keymap.set('v', '<', '<gv', { noremap = true, desc = 'Visual mode un-indent' })
vim.keymap.set('v', '>', '>gv', { noremap = true, desc = 'Visual mode indent' })

--vim.keymap.set('n', '<leader>ee', ':Lex 30<CR>', { desc = 'Open left-side explorer window' })
--vim.keymap.set('v', '<leader>ee', ':Lex 30<CR>', { desc = 'Open left-side explorer window' })
--vim.keymap.set('n', '\\', ':Lex 30<CR>', { desc = 'Open left-side explorer window' })
--vim.keymap.set('v', '\\', ':Lex 30<CR>', { desc = 'Open left-side explorer window' })


-- [[ Autocommands ]]
--  See `:help lua-guide-autocommands`

-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.hl.on_yank()`
vim.api.nvim_create_autocmd('TextYankPost', {
    desc = 'Highlight when yanking (copying) text',
    group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
    callback = function()
        vim.hl.on_yank()
    end,
})

-- [[ Filetype specific options ]]
vim.api.nvim_create_autocmd('FileType', {
    pattern = 'go',
    callback = function()
        vim.bo.tabstop = 4
        vim.bo.expandtab = false
    end,
})

vim.api.nvim_create_autocmd('FileType', {
    pattern = 'markdown',
    callback = function()
        vim.bo.tabstop = 2
        vim.bo.shiftwidth = 2
        vim.bo.softtabstop = 2
        vim.bo.expandtab = true
    end,
})

vim.api.nvim_create_autocmd('FileType', {
    pattern = 'make',
    callback = function()
        vim.bo.tabstop = 4
        vim.bo.shiftwidth = 4
        vim.bo.softtabstop = 4
        vim.bo.expandtab = false
    end,
})

-- [[ Packages ]]
-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
            { out, "WarningMsg" },
            { "\nPress any key to exit..." },
        }, true, {})
        vim.fn.getchar()
        os.exit(1)
    end
end
vim.opt.rtp:prepend(lazypath)

-- Setup lazy.nvim
require("lazy").setup({
    {
        'nvim-telescope/telescope.nvim', version = '*',
        dependencies = {
            'nvim-lua/plenary.nvim',
            -- optional but recommended
            -- { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
        },
        config = function()
            local builtin = require('telescope.builtin')
            vim.keymap.set('n', '<leader>ff', builtin.find_files, { desc = 'Telescope find files' })
            vim.keymap.set('n', '<leader>fg', builtin.live_grep, { desc = 'Telescope live grep' })
            vim.keymap.set('n', '<leader>fb', builtin.buffers, { desc = 'Telescope buffers' })
            vim.keymap.set('n', '<leader>fh', builtin.help_tags, { desc = 'Telescope help tags' })
        end,
    },

    {
        "nvim-neo-tree/neo-tree.nvim",
        branch = "v3.x",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "MunifTanjim/nui.nvim",
            "nvim-tree/nvim-web-devicons",
        },
        lazy = false, -- neo-tree will lazily load itself
        config = function()
            require('neo-tree').setup({
                filesystem = {
                    filtered_items = {
                        visible = true,
                        hide_dotfiles = false,
                        hide_gitignored = false,
                        hide_hidden = false,
                        never_show = {},
                    },
                },
            })

            vim.keymap.set('n', 'g\\', ':Neotree ', {})
            vim.keymap.set('v', 'g\\', ':Neotree ', {})

            vim.keymap.set('n', '\\', ':Neotree toggle<CR>', {})
            vim.keymap.set('v', '\\', ':Neotree toggle<CR>', {})

            vim.keymap.set('n', '<leader>e', ':Neotree reveal_force_cwd<CR>', {})
            vim.keymap.set('v', '<leader>e', ':Neotree reveal_force_cwd<CR>', {})
        end
    },

    {
        'nvim-mini/mini.comment',
        version = false,
        opts = {
            mappings = {
                comment_line = '<a-;>',
                comment_visual = '<a-;>',
            },
        },
    },

    {
        'rose-pine/neovim',
        name = 'rose-pine',
        opts = {
            highlight_groups = {
                -- NonText = { fg = 'base' }
            },
        },
    },

    {
        'junegunn/goyo.vim',
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
                end,
            })
            vim.api.nvim_create_autocmd("User", {
                pattern = "GoyoLeave",
                callback = function()
                    vim.o.showmode = true
                    vim.o.showcmd = true
                    vim.o.scrolloff = 5
                end,
            })
        end,
    },

    {
        'nvim-treesitter/nvim-treesitter',
        lazy = false,
        build = ':TSUpdate',
        config = function()
            require('nvim-treesitter').install({
                'bash',
                'c',
                'cpp',
                'markdown',
                'markdown_inline',
                'odin',
                'vim',
                'vimdoc',
            })
            vim.api.nvim_create_autocmd('FileType', {
                callback = function(args)
                    local buf, filetype = args.buf, args.match

                    local language = vim.treesitter.language.get_lang(filetype)
                    if not language then return end

                    -- check if parser exists and load it
                    if not vim.treesitter.language.add(language) then return end
                    -- enables syntax highlighting and other treesitter features
                    vim.treesitter.start(buf, language)

                    -- enables treesitter based folds
                    -- for more info on folds see `:help folds`
                    -- vim.wo.foldexpr = 'v:lua.vim.treesitter.foldexpr()'
                    -- vim.wo.foldmethod = 'expr'

                    -- enables treesitter based indentation
                    vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
                end,
            })
        end,
    },
})

require('local')
