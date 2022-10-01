local fn = vim.fn
-- Automatically install packer
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
	PACKER_BOOTSTRAP = fn.system({
		"git",
		"clone",
		"--depth",
		"1",
		"https://github.com/wbthomason/packer.nvim",
		install_path,
	})
	print("Installing packer close and reopen Neovim...")
	vim.cmd([[packadd packer.nvim]])
end

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd([[
  augroup packer_user_config
  autocmd!
  autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]])

-- Use a protected call so we don't error out on first use
local status_ok, packer = pcall(require, "packer")
if not status_ok then
	return
end

-- Have packer use a popup window
packer.init({
	display = {
		open_fn = function()
			return require("packer.util").float({ border = "rounded" })
		end,
	},
})

-- Install your plugins here
return packer.startup(function(use)
	-- My plugins here
	use("wbthomason/packer.nvim") -- Have packer manage itself
	use("nvim-lua/popup.nvim") -- An implementation of the Popup API from vim in Neovim
	use("nvim-lua/plenary.nvim") -- Useful lua functions used ny lots of plugins

	-- Colorschemes
	--
	use("arcticicestudio/nord-vim")
	use("fielding/vice")
	use("folke/tokyonight.nvim")
	use("sainnhe/sonokai")
	use("joshdick/onedark.vim")
	use("pineapplegiant/spaceduck")
	use("muchzill4/doubletrouble")
	use({ "catppuccin/nvim", as = "catppuccin" })

	-- LSP
	use("williamboman/nvim-lsp-installer") -- simple to use language server installer
	use("neovim/nvim-lspconfig") -- enable LSP
	use("tamago324/nlsp-settings.nvim") -- language server settings defined in json for
	use("jose-elias-alvarez/null-ls.nvim") -- for formatters and linters

	-- pretty lsp
	use({
		"folke/trouble.nvim",
		requires = "kyazdani42/nvim-web-devicons",
	})

	-- cmp plugins
	use("hrsh7th/nvim-cmp") -- The completion plugin
	use("hrsh7th/cmp-buffer") -- buffer completions
	use("hrsh7th/cmp-path") -- path completions
	use("hrsh7th/cmp-cmdline") -- cmdline completions
	use("hrsh7th/cmp-nvim-lua") -- lua completions
	use("saadparwaiz1/cmp_luasnip") -- snippet completions
	use("hrsh7th/cmp-nvim-lsp") -- lsp completion

	-- snippets
	use("L3MON4D3/LuaSnip") --snippet engine
	use("rafamadriz/friendly-snippets") -- a bunch of snippets to use

	-- Improve lua module loading time
	use("lewis6991/impatient.nvim")


	-- mkdir
	use("jghauser/mkdir.nvim")
	-- autopairs
	use("windwp/nvim-autopairs")
	-- autotag
	--use("windwp/nvim-ts-autotag")

	-- reverse j
	use({
		"AckslD/nvim-trevJ.lua",
		config = 'require("trevj").setup()',
		setup = function()
			vim.keymap.set("n", "<leader>j", function()
				require("trevj").format_at_cursor()
			end)
		end,
	})

	-- Comment
	use("numToStr/Comment.nvim")
	use("JoosepAlviste/nvim-ts-context-commentstring") -- plugin that help for better commenting
	-- vim-surround
	use("tpope/vim-surround")

	--Indentaion
	use("lukas-reineke/indent-blankline.nvim")

	-- Treesitter
	use({
		"nvim-treesitter/nvim-treesitter",
		run = "TSUpdate",
	})
	use("p00f/nvim-ts-rainbow") -- a rainbow ts

	use("nvim-treesitter/nvim-treesitter-context")
	use("nvim-treesitter/playground")

	-- buffdelete
	use("famiu/bufdelete.nvim")

	-- bufferline
	use({ "akinsho/bufferline.nvim", tag = "*", requires = "kyazdani42/nvim-web-devicons" })

	-- lualine
	use("nvim-lualine/lualine.nvim")

	-- colorizer
	use("norcalli/nvim-colorizer.lua")

	-- toggleterm
	use({ "akinsho/toggleterm.nvim" })

	-- smooth scrolling
	use("karb94/neoscroll.nvim")
    
	-- Telescope
	use({
		"nvim-telescope/telescope.nvim",
		requires = { { "nvim-lua/plenary.nvim" } },
	})
	-- telescope extension --
	use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" }) -- fzf written in C
	-- use 'nvim-telescope/telescope-media-files.nvim' -- media previewer
	use("nvim-telescope/telescope-ui-select.nvim") --
	-- symbols source for telescope
	use("nvim-telescope/telescope-symbols.nvim")
	use("kyazdani42/nvim-web-devicons") -- devicone
	-- nvim-tree
	use("kyazdani42/nvim-tree.lua")

	use({
		"lewis6991/gitsigns.nvim",
		-- tag = 'release' -- To use the latest release
	})

	-- Automatically set up your configuration after cloning packer.nvim

	-- Automatically set up your configuration after cloning packer.nvim
	-- Put this at the end after all plugins
	if PACKER_BOOTSTRAP then
		require("packer").sync()
	end
end)
