-- Neo-tree is a Neovim plugin to browse the file system
-- https://github.com/nvim-neo-tree/neo-tree.nvim

return {
    'nvim-neo-tree/neo-tree.nvim',
    version = '*',
    dependencies = {
        'nvim-lua/plenary.nvim',
        'nvim-tree/nvim-web-devicons', -- not strictly required, but recommended
        'MunifTanjim/nui.nvim',
    },
    cmd = 'Neotree',
    keys = {
        { '\\', ':Neotree reveal_force_cwd reveal<CR>', desc = 'NeoTree reveal', silent = true },
    },
    opts = {
        bind_to_cwd = true, -- true creates a 2-way binding between vim's cwd and neo-tree's root
        filesystem = {
            window = {
                mappings = {
                    ['\\'] = 'close_window',
                },
            },
        },
        close_if_last_window = true,
    },
}
