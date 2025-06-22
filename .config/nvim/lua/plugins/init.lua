return {

    -- My plugins here
    "nvim-lua/popup.nvim",   -- An implementation of the Popup API from vim in Neovim
    "nvim-lua/plenary.nvim", -- Useful lua functions used ny lots of plugins

    -- Colorschemes
	{
		'ntk148v/komau.vim'
	},
	{
		'thimc/gruber-darker.nvim',
		config = function()
			require('gruber-darker').setup({
				-- OPTIONAL
				transparent = true, -- removes the background
				-- underline = false, -- disables underline fonts
				-- bold = false, -- disables bold fonts
			})
		end,
	},
	{
		"sainnhe/sonokai",
		lazy = true,
	},

	{
		"rebelot/kanagawa.nvim",
		lazy = true,
		opts = {
			undercurl = true, -- enable undercurls
			commentStyle = { italic = true },
			functionStyle = {},
			keywordStyle = { italic = true },
			statementStyle = { bold = true },
			typeStyle = {},
			variablebuiltinStyle = { italic = true },
			specialReturn = true,    -- special highlight for the return keyword
			specialException = true, -- special highlight for exception handling keywords
			transparent = true,     -- do not set background color
			dimInactive = false,     -- dim inactive window `:h hl-NormalNC`
			globalStatus = false,    -- adjust window separators highlight for laststatus=3
			terminalColors = true,   -- define vim.g.terminal_color_{0,17}
			colors = {},
			theme = "default",       -- Load "default" theme or the experimental "light" theme
		},
		priority = 100,
	},

	{
		"joshdick/onedark.vim",
		lazy = true,
	},

	-- tmux-vim navigation
	{
		"christoomey/vim-tmux-navigator",
		cmd = {
			"TmuxNavigateLeft",
			"TmuxNavigateDown",
			"TmuxNavigateUp",
			"TmuxNavigateRight",
			"TmuxNavigatePrevious",
		},
		keys = {
			{ "<c-h>", "<cmd><C-U>TmuxNavigateLeft<cr>" },
			{ "<c-j>", "<cmd><C-U>TmuxNavigateDown<cr>" },
			{ "<c-k>", "<cmd><C-U>TmuxNavigateUp<cr>" },
			{ "<c-l>", "<cmd><C-U>TmuxNavigateRight<cr>" },
			{ "<c-\\>", "<cmd><C-U>TmuxNavigatePrevious<cr>" },
		},
	},

	-- snippets
	"rafamadriz/friendly-snippets", -- a bunch of snippets to use

	-- autopairs
	{
		'windwp/nvim-autopairs',
		event = "InsertEnter",
		config = true
		-- use opts = {} for passing setup options
		-- this is equalent to setup({}) function
	},


	-- nvim-surround
	{
		"kylechui/nvim-surround",
		version = "*",
		event = "VeryLazy",
		config = function()
			require("nvim-surround").setup({
				-- Configuration here, or leave empty to use defaults
			})
		end
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

	-- telescope extension --
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build = "make", -- fzf written in C,
		lazy = true,
	},

	"kyazdani42/nvim-web-devicons", -- devicone
	{
		'renerocksai/telekasten.nvim',
		opts = {
			home = vim.fn.expand("~/zettel"),
		}
	},
}
