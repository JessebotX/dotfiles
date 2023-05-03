local autocmd = vim.api.nvim_create_autocmd

autocmd('Filetype', {
  pattern = { 'markdown', 'text' },
  command = 'setlocal textwidth=60 conceallevel=2'
})

autocmd('Filetype', {
  pattern = {
    'markdown',
    'xml',
    'html',
    'xhtml',
    'css',
    'scss',
    'javascript',
    'typescript',
    'typescriptreact',
    'javascriptreact',
    'yaml',
    'lua',
    'json',
    'toml'
  },
  command = 'setlocal tabstop=2 softtabstop=2 expandtab'
})
