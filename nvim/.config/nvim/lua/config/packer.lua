local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
  -- Packer can manage itself
  use('wbthomason/packer.nvim')

  -- Themes
  use({ "catppuccin/nvim", as = "catppuccin"})
  use 'ishan9299/modus-theme-vim'

  -- Treesitter
  use('nvim-treesitter/nvim-treesitter')
  use('nvim-treesitter/playground')

  -- LSP + Autocompletion
  use({
    'VonHeikemen/lsp-zero.nvim',
    branch = 'v2.x',
    requires = {
      {'neovim/nvim-lspconfig'},
      {
        'williamboman/mason.nvim',
        run = function()
          pcall(vim.cmd, 'MasonUpdate')
        end,
      },
      {'williamboman/mason-lspconfig.nvim'},

      -- Autocompletion
      {'hrsh7th/nvim-cmp'},
      {'hrsh7th/cmp-nvim-lsp'},
      {'L3MON4D3/LuaSnip'},
    }
  })
  use("folke/neodev.nvim")

  -- Fuzzy Finder
  use({
    'nvim-telescope/telescope.nvim', tag = '0.1.1',
    requires = { {'nvim-lua/plenary.nvim'} }
  })

  -- Undo
  use('mbbill/undotree')

  -- Git & Version Control
  use('tpope/vim-fugitive')

  -- Neovim Icons
  use 'nvim-tree/nvim-web-devicons'

  -- Tab bar
  use {'romgrk/barbar.nvim', requires = 'nvim-web-devicons'}

  -- Modeline
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'nvim-tree/nvim-web-devicons', opt = true }
  }

  -- File tree
  use {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v2.x",
    requires = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons",
      "MunifTanjim/nui.nvim",
    }
  }

  -- Markdown preview
  use({
      "iamcco/markdown-preview.nvim",
      run = function() vim.fn["mkdp#util#install"]() end,
  })

  -- Writing
  use "folke/zen-mode.nvim"

  use 'numToStr/Comment.nvim'

  use 'folke/which-key.nvim'
end)
