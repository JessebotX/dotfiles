local map = vim.api.nvim_set_keymap
local opts = { noremap = true, silent = true }

-- Move to previous/next
map('n', '<leader>h', '<Cmd>BufferPrevious<CR>', opts)
map('n', '<leader>l', '<Cmd>BufferNext<CR>', opts)

-- Re-order to previous/next
map('n', '<A-h>', '<Cmd>BufferMovePrevious<CR>', opts)
map('n', '<A-l>', '<Cmd>BufferMoveNext<CR>', opts)

-- Close buffer
map('n', '<leader>-', '<Cmd>BufferClose<CR>', opts)
