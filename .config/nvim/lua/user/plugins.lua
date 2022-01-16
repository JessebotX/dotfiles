local fn = vim.fn

-- Automatically install packer
local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system {
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	}
	print "Installing packer close and reopen Neovim..."
	vim.cmd [[packadd packer.nvim]]
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
augroup packer_user_config
	autocmd!
	autocmd BufWritePost plugins.lua source <afile> | PackerSync
augroup end
]]

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	return
end

-- Have packer use a popup window
packer.init {
	display = {
		open_fn = function()
			return require("packer.util").float { border = "rounded" }
		end,
	},
}

-- SOME USEFUL PACKER COMMANDS
-- - :PackerStatus shows installed
-- - :PackerUpdate update plugins
-- - :PackerSync   update and compile things to speed up packer
-- Install your plugins here
return packer.startup(function(use)
	-- My plugins here

	use "wbthomason/packer.nvim" -- Have packer manage itself

	use "nvim-lua/popup.nvim" -- An implementation of the Popup API from vim in Neovim
	use "nvim-lua/plenary.nvim" -- Useful lua functions used ny lots of plugins

	use "sainnhe/gruvbox-material"
	use "windwp/nvim-autopairs"
	use "lewis6991/gitsigns.nvim"
	use "akinsho/bufferline.nvim"
	use 'kyazdani42/nvim-tree.lua'
	use 'nvim-lualine/lualine.nvim'
	use "nvim-telescope/telescope.nvim"
	use 'nvim-telescope/telescope-media-files.nvim'
	use 'goolord/alpha-nvim'
	use "ahmedkhalf/project.nvim"
	use 'famiu/bufdelete.nvim'

	-- Automatically set up your configuration after cloning packer.nvim
	-- Put this at the end after all plugins
	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)

