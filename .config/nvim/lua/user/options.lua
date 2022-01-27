local opt = vim.opt
local options = {
	fileencoding     = "utf-8",
	clipboard        = "unnamedplus",
	termguicolors    = true,
	swapfile         = false,
	backup           = false,
	writebackup      = false,
	cmdheight        = 2,
	completeopt      = { "menuone", "noselect" },
	conceallevel     = 2,
	hlsearch         = true,
	smartcase        = true,
	ignorecase       = true,
	mouse            = "a",
	-- pop up menu height
	pumheight        = 10,
	showmode         = false,
	showtabline      = 2,
	splitbelow       = true,
	splitright       = true,
	timeoutlen       = 1000,
	undofile         = true,
	updatetime       = 300,
	--expandtab        = false,
	shiftwidth       = 0,
	tabstop          = 8,
	smartindent      = true,
	cursorline       = false,
	numberwidth      = 4,
	signcolumn       = "no",
	wrap             = true,
	scrolloff        = 5,
	sidescrolloff    = 5,
	textwidth        = 72,
	number           = true,
	relativenumber   = true,
}


opt.shortmess:append "c"
opt.iskeyword:append "-"

for k, v in pairs(options) do
	opt[k] = v
end

vim.cmd [[
set colorcolumn=72
]]

-- color scheme
vim.cmd [[
try
	set termguicolors
	let g:gruvbox_material_enable_italic = 1
	let g:gruvbox_material_background="soft"
	set background=dark
	colorscheme onedark
	"hi! EndOfBuffer ctermbg=bg ctermfg=bg guibg=bg guifg=bg
	hi link CursorLine CursorColumn
catch /^Vim\%((\a\+)\)\=:E185/
	colorscheme default
	set background=dark
endtry
]]

-- netrw
vim.cmd[[
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_winsize = 15
]]
