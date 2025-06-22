---@diagnostic disable: undefined-global
return {

	"nvim-telescope/telescope.nvim",
	tag = '0.1.8',
	config = function()
		local telescope = require("telescope")
		local telescopeConfig = require("telescope.config")
		local vimgrep_arguments = { unpack(telescopeConfig.values.vimgrep_arguments) }

		-- I want to search in hidden/dot files.
		table.insert(vimgrep_arguments, "--hidden")
		-- I don't want to search in the `.git` directory.
		table.insert(vimgrep_arguments, "--glob")
		table.insert(vimgrep_arguments, "!**/.git/*")
		local actions = require "telescope.actions"

		telescope.setup(
			{
				defaults = {
					-- `hidden = true` is not supported in text grep commands.
					layout_strategy = "horizontal",
					sorting_strategy = "ascending",
					selection_strategy = "reset",
					--file_ignore_patterns = {
					--	"node%_modules/.*",
					--	"%.pdf",
					--	"%.png",
					--	"%.jpg",
					--	"%.djvu",
					--	"%.bin",
					--	"%.tar",
					--	"%.iso",
					--	"%.js",
					--	"%.mp3",
					--	"%.mp4",
					--	"%.mkv",
					--	"%.out",
					--	"%.o",
					--},
					layout_config = {
						horizontal = {
							prompt_position = "top",
							preview_width = 0.55,
							results_width = 0.8,
							anchor = "N",
						},
						vertical = {
							mirror = false,
						},
						width = 0.87,
						height = 0.80,
						preview_cutoff = 120,
					},
					prompt_title = "vim",
					prompt_prefix = " ",
					selection_caret = " ",
					multi_icon = "",
					border = true,
					scroll_strategy = "cycle",
					vimgrep_arguments = vimgrep_arguments,
					path_display = { "truncate" },
					winblend = 0,
					color_devicons = true,
					set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
					file_previewer = require("telescope.previewers").vim_buffer_cat.new,
					grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
					qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
					mappings = {
						n = {
							["<esc>"] = actions.close,
							["<cr>"] = actions.select_default,
							["<c-s>"] = actions.select_horizontal,
							["<c-v>"] = actions.select_vertical,
							["<c-t>"] = actions.select_tab,

							["<tab>"] = actions.toggle_selection + actions.move_selection_worse,
							["<s-tab>"] = actions.toggle_selection + actions.move_selection_better,
							["<c-q>"] = actions.send_to_qflist + actions.open_qflist,
							["<m-q>"] = actions.send_selected_to_qflist + actions.open_qflist,

							["j"] = actions.move_selection_next,
							["k"] = actions.move_selection_previous,
							["h"] = actions.move_to_top,
							["m"] = actions.move_to_middle,
							["l"] = actions.move_to_bottom,

							["<down>"] = actions.move_selection_next,
							["<up>"] = actions.move_selection_previous,
							["gg"] = actions.move_to_top,
							["g"] = actions.move_to_bottom,

							["<c-u>"] = actions.preview_scrolling_up,
							["<c-d>"] = actions.preview_scrolling_down,

							["<pageup>"] = actions.results_scrolling_up,
							["<pagedown>"] = actions.results_scrolling_down,

							["?"] = actions.which_key,
						},
						i = {
							["<C-n>"] = actions.cycle_history_next,
							["<C-p>"] = actions.cycle_history_prev,

							["<C-j>"] = actions.move_selection_next,
							["<C-k>"] = actions.move_selection_previous,

							["<C-c>"] = actions.close,

							["<Down>"] = actions.move_selection_next,
							["<Up>"] = actions.move_selection_previous,

							["<CR>"] = actions.select_default,
							["<C-s>"] = actions.select_horizontal,
							["<C-v>"] = actions.select_vertical,
							["<C-t>"] = actions.select_tab,

							["<C-u>"] = actions.preview_scrolling_up,
							["<C-d>"] = actions.preview_scrolling_down,

							["<PageUp>"] = actions.results_scrolling_up,
							["<PageDown>"] = actions.results_scrolling_down,

							["<Tab>"] = actions.toggle_selection + actions.move_selection_worse,
							["<S-Tab>"] = actions.toggle_selection + actions.move_selection_better,
							["<C-q>"] = actions.send_to_qflist + actions.open_qflist,
							["<M-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
							["<C-l>"] = actions.complete_tag,

						},
					},
				},
				extensions = {
				--	fzf = {
				--		fuzzy = true,                   -- false will only do exact matching
				--		override_generic_sorter = true, -- override the generic sorter
				--		override_file_sorter = true,    -- override the file sorter
				--		case_mode = "smart_case",       -- or "ignore_case" or "respect_case"
				--		-- the default case_mode is "smart_case"
				--	}
				}

			}
		)
		--require('telescope').load_extension('fzf')
--		require('telescope').load_extension('ui-select')
	end
	,
	dependencies = {
		"nvim-lua/plenary.nvim",
		"nvim-telescope/telescope-ui-select.nvim",
	},
	keys = {
		{ "<leader>tl", "<cmd>Telescope<cr>", desc = "telescop" },
		{
			"<leader>ff",
			"<cmd>:lua require('telescope.builtin').find_files()<cr>",
			desc =
				"find files picker"
		},
		{
			"<leader>fg",
			"<cmd>lua require('telescope.builtin').live_grep()<CR>",
			desc =
				"live grep picker"
		},
		{
			"<leader>fo",
			"<cmd>:lua require('telescope.builtin').oldfiles()<cr>",
			desc =
				"live grep picker"
		},
		{
			"<leader>bf",
			"<cmd>:lua require('telescope.builtin').buffers()<cr>",
			desc =
				"live grep picker"
		},
		{
			"<leader>fw",
			"<cmd>:lua require('telescope.builtin').grep_string()<cr>",
			desc =
				"Find Word under Cursor"
		},
		{
			'<leader>gf',
			"<cmd>:lua require('telescope.builtin').git_files()<cr>",
			desc =
				"find git files"
		},
	},
	cmd = "Telescope"
}
