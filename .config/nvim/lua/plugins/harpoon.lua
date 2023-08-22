return {
	"ThePrimeagen/harpoon",
	opts = {
		global_settings = {
			-- sets the marks upon calling `toggle` on the ui, instead of require `:w`.
			save_on_toggle = true,

			-- saves the harpoon file upon every change. disabling is unrecommended.
			save_on_change = true,

			-- sets harpoon to run the command immediately as it's passed to the terminal when calling `sendCommand`.
			enter_on_sendcmd = false,

			-- closes any tmux windows harpoon that harpoon creates when you close Neovim.
			tmux_autoclose_windows = false,

			-- filetypes that you want to prevent from adding to the harpoon list menu.
			excluded_filetypes = { "harpoon" },

			-- set marks specific to each git branch inside git repository
			mark_branch = false,
		},
	},
	keys = {
		{ "<leader>m", "<cmd>:lua require('harpoon.mark').add_file()<CR>", desc = "harpoon" },
		{ "<leader>h", "<cmd>:lua require('harpoon.ui').toggle_quick_menu()<CR>", desc = "harpoon" },
		{ "<A-1>", '<cmd>:lua require("harpoon.ui").nav_file(1)<CR>' },
		{ "<A-2>", '<cmd>:lua require("harpoon.ui").nav_file(2)<CR>' },
		{ "<A-3>", '<cmd>:lua require("harpoon.ui").nav_file(3)<CR>' },
		{ "<A-4>", '<cmd>:lua require("harpoon.ui").nav_file(4)<CR>' },
		{ "<A-5>", '<cmd>:lua require("harpoon.ui").nav_file(5)<CR>' },
		{ "<A-6>", '<cmd>:lua require("harpoon.ui").nav_file(6)<CR>' },
		{ "<A-7>", '<cmd>:lua require("harpoon.ui").nav_file(7)<CR>' },
		{ "<A-8>", '<cmd>:lua require("harpoon.ui").nav_file(8)<CR>' },
	},
}
