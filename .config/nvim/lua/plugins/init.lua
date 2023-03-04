return {

	-- My plugins here
	"nvim-lua/popup.nvim", -- An implementation of the Popup API from vim in Neovim
	"nvim-lua/plenary.nvim", -- Useful lua functions used ny lots of plugins

	-- Colorschemes
	--
	{
		"Tsuzat/NeoSolarized.nvim",
	},
	"arcticicestudio/nord-vim",
	{ "sainnhe/sonokai" },
	{
		"rebelot/kanagawa.nvim",
		opts = {
			undercurl = true, -- enable undercurls
			commentStyle = { italic = true },
			functionStyle = {},
			keywordStyle = { italic = true },
			statementStyle = { bold = true },
			typeStyle = {},
			variablebuiltinStyle = { italic = true },
			specialReturn = true, -- special highlight for the return keyword
			specialException = true, -- special highlight for exception handling keywords
			transparent = false, -- do not set background color
			dimInactive = false, -- dim inactive window `:h hl-NormalNC`
			globalStatus = false, -- adjust window separators highlight for laststatus=3
			terminalColors = true, -- define vim.g.terminal_color_{0,17}
			colors = {},
			overrides = {},
			theme = "default", -- Load "default" theme or the experimental "light" theme
		},
		priority = 100,
	},

	{ "joshdick/onedark.vim" },

	-- LSP
	{
		"williamboman/mason.nvim",
	},

	-- enable LSP
	{
		"neovim/nvim-lspconfig",
	},
	"jose-elias-alvarez/null-ls.nvim", -- for formatters and linters

	-- DAP (Debuge Adapter Protocol)
	"mfussenegger/nvim-dap",

	-- pretty lsp
	{
		"folke/trouble.nvim",
		dependencies = "kyazdani42/nvim-web-devicons",
		cmd = "Trouble",
		keys = {
			{ "<leader>q", "<cmd>TroubleToggle<cr>", desc = "trouble" },
		},
		opts = {},
		lazy = true,
	},

	-- cmp plugins
	"hrsh7th/nvim-cmp", -- The completion plugin
	"hrsh7th/cmp-buffer", -- buffer completions
	"hrsh7th/cmp-path", -- path completions
	"hrsh7th/cmp-cmdline", -- cmdline completions
	"hrsh7th/cmp-nvim-lua", -- lua completions
	"saadparwaiz1/cmp_luasnip", -- snippet completions
	"hrsh7th/cmp-nvim-lsp", -- lsp completion

	-- tmux-vim navigation
	{
		"christoomey/vim-tmux-navigator",
		config = function()
			vim.cmd([[
        let g:tmux_navigator_save_on_switch = 1
        let g:tmux_navigator_disable_when_zoomed = 1
        ]])
		end,
	},

	-- snippets
	"L3MON4D3/LuaSnip", --snippet engine
	"rafamadriz/friendly-snippets", -- a bunch of snippets to use

	-- Improve lua module loading time
	"lewis6991/impatient.nvim",
	-- autopairs
	"windwp/nvim-autopairs",
	-- autotag
	{
		"windwp/nvim-ts-autotag",
	},

	-- Comment
	{
		"numToStr/Comment.nvim",
		keys = {
			{ "gcc", "<Plug>(comment_toggle_linewise_curretn)", desc = "comment" },
		},
		lazy = true,
	},
	"JoosepAlviste/nvim-ts-context-commentstring", -- plugin that help for better commenting
	-- vim-surround
	"tpope/vim-surround",
	"tpope/vim-repeat",

	--Indentaion
	"lukas-reineke/indent-blankline.nvim",

	--NvimTree
	{
		"windwp/nvim-ts-autotag",
	},

	-- Treesitter
	{
		"nvim-treesitter/nvim-treesitter",
		build = "TSUpdate",
	},

	"p00f/nvim-ts-rainbow", -- a rainbow ts

	"nvim-treesitter/nvim-treesitter-context",
	{
		"nvim-treesitter/playground",
		cmd = "TSPlaygroundToggle",
		lazy = true,
	},

	-- buffdelete
	{
		"famiu/bufdelete.nvim",
		keys = {
			{
				"<leader>bd",
				"<cmd>:lua require('bufdelete').bufdelete(0, true)<cr>",
				desc = "delete vim buffer without loosing the layout",
			},
		},
	},

	-- bufferline
	{
		"akinsho/bufferline.nvim",
		dependencies = "kyazdani42/nvim-web-devicons",
	},

	-- lualine
	"nvim-lualine/lualine.nvim",

	-- colorizer
	{
		"norcalli/nvim-colorizer.lua",
		cmd = "ColorizerAttachToBuffer",
		ft = "css",
	},

	-- smooth scrolling
	"karb94/neoscroll.nvim",

	-- telescope extension --
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build = "make", -- fzf written in C,
	},

	"kyazdani42/nvim-web-devicons", -- devicone
	{
		"lewis6991/gitsigns.nvim",
		tag = "release", -- To use the latest release
	},
    
    
}
