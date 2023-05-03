vim.opt.fileencoding = "utf-8"
vim.opt.termguicolors = true

vim.opt.cmdheight = 2
vim.opt.clipboard = "unnamedplus"
vim.opt.guicursor = ""
vim.opt.scrolloff = 5
vim.opt.showtabline = 2

vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.smartcase = true
vim.opt.ignorecase = true

vim.opt.splitright = true
vim.opt.splitbelow = true

vim.opt.linebreak = true
vim.opt.wrap = true

vim.opt.smartindent = true
vim.opt.tabstop = 8
vim.opt.softtabstop = 8
vim.opt.shiftwidth = 0
vim.opt.expandtab = false

vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.showmode = false

vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.opt.undofile = true

vim.opt.updatetime = 50

vim.opt.conceallevel = 0

vim.opt.listchars = { space = ' ', tab = '→ ' }
vim.opt.list = true
