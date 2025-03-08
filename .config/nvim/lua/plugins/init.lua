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

	-- LSP
	{
		"williamboman/mason.nvim",
	},
	"williamboman/mason-lspconfig.nvim",

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


	{
		"windwp/nvim-ts-autotag",
		lazy = true,
		opts = {
			enable = true,
			filetype = { "html", "xml", "python" },
		},
		ft = { "html", "xml", "python" },

	},


	{
		"nvim-treesitter/nvim-treesitter-context",
		--        event = "BufWinEnter",
		opts = {
			enable = true,         -- Enable this plugin (Can be enabled/disabled later via commands)
			max_lines = 1,         -- How many lines the window should span. Values <= 0 mean no limit.
			trim_scope = "outer",  -- Which context lines to discard if `max_lines` is exceeded. Choices: 'inner', 'outer'
			line_numbers = true,
			multiline_threshold = 20, -- Maximum number of lines to show for a single context
			min_window_height = 0, -- Minimum editor window height to enable context. Values <= 0 mean no limit.
			zindex = 20,     -- The Z-index of the context window
			mode = "cursor", -- Line used to calculate context. Choices: 'cursor', 'topline'
			-- Separator between context and content. Should be a single character string, like '-'.
			-- When separator is set, the context will only show up when there are at least 2 lines above cursorline.
			separator = nil,
			on_attach = nil,

		}
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

	-- lualine
	--    "nvim-lualine/lualine.nvim",


	-- telescope extension --
	{
		"nvim-telescope/telescope-fzf-native.nvim",
		build = "make", -- fzf written in C,
		lazy = true,
	},

	"kyazdani42/nvim-web-devicons", -- devicone
	"lukas-reineke/cmp-rg",
	-- markdown previewer

	{
		"folke/zen-mode.nvim",

		opts = {
			window = {
				backdrop = 0.95, -- shade the backdrop of the Zen window. Set to 1 to keep the same as Normal
				-- height and width can be:
				-- * an absolute number of cells when > 1
				-- * a percentage of the width / height of the editor when <= 1
				-- * a function that returns the width or the height
				width = 120, -- width of the Zen window
				height = 1,  -- height of the Zen window
				-- by default, no options are changed for the Zen window
				-- uncomment any of the options below, or add other vim.wo options you want to apply
				options = {
					-- signcolumn = "no", -- disable signcolumn
					-- number = false, -- disable number column
					-- relativenumber = false, -- disable relative numbers
					-- cursorline = false, -- disable cursorline
					-- cursorcolumn = false, -- disable cursor column
					-- foldcolumn = "0", -- disable fold column
					-- list = false, -- disable whitespace characters
				},
			},
			plugins = {
				-- disable some global vim options (vim.o...)
				-- comment the lines to not apply the options
				options = {
					enabled = true,
					ruler = false,              -- disables the ruler text in the cmd line area
					showcmd = false,            -- disables the command in the last line of the screen
				},
				twilight = { enabled = true },  -- enable to start Twilight when zen mode opens
				gitsigns = { enabled = false }, -- disables git signs
				tmux = { enabled = true },      -- disables the tmux statusline
				-- this will change the font size on kitty when in zen mode
				-- to make this work, you need to set the following kitty options:
				-- - allow_remote_control socket-only
				-- - listen_on unix:/tmp/kitty
				kitty = {
					enabled = false,
					font = "+4", -- font size increment
				},
				-- this will change the font size on alacritty when in zen mode
				-- requires  Alacritty Version 0.10.0 or higher
				-- uses `alacritty msg` subcommand to change font size
				alacritty = {
					enabled = false,
					font = "14", -- font size
				},
				-- this will change the font size on wezterm when in zen mode
				-- See alse also the Plugins/Wezterm section in this projects README
				wezterm = {
					enabled = false,
					-- can be either an absolute font size or the number of incremental steps
					font = "+4", -- (10% increase per step)
				},
			},
			-- callback where you can add custom code when the Zen window opens
			on_open = function(win)
			end,
			-- callback where you can add custom code when the Zen window closes
			on_close = function()
			end,
		},
		keys = { {
			"<leader>z",
			"<cmd>ZenMode<CR>",
			desc = "toggle zen mode"
		} },
		lazy = true,
	},
}
