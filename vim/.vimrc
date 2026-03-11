let mapleader = ' '
let maplocalleader = ' '

" ----
" Plugins
" ----

call plug#begin()

Plug 'preservim/nerdtree'
Plug 'ayu-theme/ayu-vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'

call plug#end()

" ----
" Colorscheme
" ----

set background=dark
set termguicolors
let ayucolor='dark'
silent! colorscheme ayu

" ----
" Options
" ----

filetype plugin indent on
syntax on

set nocompatible
set backspace=indent,eol,start
set clipboard=unnamed,unnamedplus
set belloff=all
set guicursor+=a:blinkon0
set guioptions=i
set guifont=Maple\ Mono\ NF:h14
set complete-=i

set wildmenu

set ruler
set number
set relativenumber

set tabstop=4
set shiftwidth=0
set softtabstop=-1
set expandtab
set smartindent

set ignorecase
set smartcase
set hlsearch
set incsearch

set history=1000
set undolevels=1000
set noswapfile
set nobackup

set laststatus=2
set scrolloff=2

set linebreak
set wrap

nmap <Esc> <cmd>nohlsearch<CR>

nmap <expr> j v:count == 0 ? 'gj' : 'j'
nmap <expr> k v:count == 0 ? 'gk' : 'k'

nmap <C-h> <C-w><C-h>
nmap <C-j> <C-w><C-j>
nmap <C-k> <C-w><C-k>
nmap <C-l> <C-w><C-l>

vnoremap < <gv
vnoremap > >gv

"
" NERDTree
"

nnoremap <C-Bslash> :NERDTreeToggle<CR>
nnoremap <leader>ee :NERDTreeToggle<CR>
vnoremap <leader>ee :NERDTreeToggle<CR>
nnoremap <leader>ej :NERDTreeFocus<CR>
vnoremap <leader>ej :NERDTreeFocus<CR>

"
" Goyo & Limelight
"

function! s:goyo_enter()
  if executable('tmux') && strlen($TMUX)
    silent !tmux set status off
    silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
  endif
  set noshowmode
  set noshowcmd
  set scrolloff=999
  Limelight
  " ...
endfunction

function! s:goyo_leave()
  if executable('tmux') && strlen($TMUX)
    silent !tmux set status on
    silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
  endif
  set showmode
  set showcmd
  set scrolloff=5
  Limelight!
  " ...
endfunction

autocmd! User GoyoEnter nested call <SID>goyo_enter()
autocmd! User GoyoLeave nested call <SID>goyo_leave()

nnoremap <leader>tw :Goyo<CR>
vnoremap <leader>tw :Goyo<CR>
