return {

    -- My plugins here
    "nvim-lua/popup.nvim",   -- An implementation of the Popup API from vim in Neovim
    "nvim-lua/plenary.nvim", -- Useful lua functions used ny lots of plugins

    -- Colorschemes
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
	
    -- jump diagnostics lsp
    {
        "folke/trouble.nvim",
        --dependencies = "kyazdani42/nvim-web-devicons",
        cmd = "Trouble",
        keys = {
            { "<leader>q", "<cmd>Trouble qflist toggle<cr>", desc = "trouble" },
        },
        opts = {},
        lazy = true,
    },

    -- tmux-vim navigation
    {
        "christoomey/vim-tmux-navigator",
        config = function()
            vim.cmd([[
            let g:tmux_navigator_save_on_switch = 1
            let g:tmux_navigator_disable_when_zoomed = 1
            let g:tmux_navigator_preserve_zoom = 1
            let  g:tmux_navigator_no_wrap = 1
            ]])
        end,
    },

    -- snippets
	"rafamadriz/friendly-snippets", -- a bunch of snippets to use

    -- autopairs
    {
        "windwp/nvim-autopairs",
        event = "InsertEnter",
        config = function()
            local npairs = require("nvim-autopairs")
            npairs.setup {
                check_ts = true,
                ts_config = {
                    lua = { "string", "source" },
                    javascript = { "string", "template_string" },
                    java = false,
                },
                disable_filetype = { "TelescopePrompt", "spectre_panel" },
                fast_wrap = {
                    map = "<M-e>",
                    chars = { "{", "[", "(", '"', "'" },
                    pattern = string.gsub([[ [%'%"%)%>%]%)%}%,] ]], "%s+", ""),
                    offset = 0, -- Offset from pattern match
                    end_key = "$",
                    keys = "qwertyuiopzxcvbnmasdfghjkl",
                    check_comma = true,
                    highlight = "PmenuSel",
                    highlight_grey = "LineNr",
                },
            }

            local cmp_autopairs = require "nvim-autopairs.completion.cmp"
            local cmp_status_ok, cmp = pcall(require, "cmp")
            if not cmp_status_ok then
                return
            end
            cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done { map_char = { tex = "" } })
        end
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
    -- "tpope/vim-repeat",

    --Indentation
    {
        "lukas-reineke/indent-blankline.nvim",
        event = "BufWinEnter",
        main = "ibl",
        config = function()
            local ibl = require("ibl")
            vim.cmd([[highlight IndentBlanklineIndent1 guifg=#0984ef gui=nocombine]])

            vim.cmd([[let g:indent_blankline_char='┆']])

            vim.opt.list = true
            vim.opt.listchars:append("eol:↴")

            ibl.setup({
                --show_end_of_line = true,
                --show_current_context = true,
                --show_current_context_start = true,
                debounce = 100,
                indent = {char = "|"},
                whitespace = {
                    highlight = { "Whitespace", "NonText" },
                    remove_blankline_trail = true,
            },
                scope = {
                    exclude = { language = { "lua" } },
                    show_start = true,
                    show_end = true,
                    highlight = { "Function", "Label" },

                },


            })
        end
    },

    --NvimTree
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

    -- colorizer
    {
        "norcalli/nvim-colorizer.lua",
        cmd = "ColorizerAttachToBuffer",
        ft = "css",
    },

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
		"toppair/peek.nvim",
		event = { "VeryLazy" },
		build = "deno task --quiet build:fast",
		config = function()
			require("peek").setup(
				{ app ='webview', }
			)
			vim.api.nvim_create_user_command("PeekOpen", require("peek").open, {})
			vim.api.nvim_create_user_command("PeekClose", require("peek").close, {})
		end,
	},

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
	{
		"nvim-telescope/telescope-bibtex.nvim",
		config = function ()
			require"telescope".load_extension("bibtex")
		end,
	},

}
