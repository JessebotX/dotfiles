require("zen-mode").setup {
  window = {
    backdrop = 1,
    width = 80,
    options = {
      signcolumn = "no", -- disable signcolumn
      number = false, -- disable number column
      relativenumber = false, -- disable relative numbers
      cursorline = false, -- disable cursorline
      cursorcolumn = false, -- disable cursor column
      foldcolumn = "0", -- disable fold column
      list = false, -- disable whitespace characters
    },
  }
}

vim.keymap.set("n", "<leader>tb", vim.cmd.ZenMode)
