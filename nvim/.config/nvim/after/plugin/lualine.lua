require('lualine').setup({
  options = {
    section_separators = { left = '', right = '' },
    component_separators = { left = '', right = '' }
  },
  sections = {
    lualine_y = { "os.date('%I:%M %p')", 'data', "require'lsp-status'.status()" }
  }
})
