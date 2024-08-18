return {
	"L3MON4D3/LuaSnip",
	dependencies = { "rafamadriz/friendly-snippets" },
	config = function ()
		local ls = require("luasnip")
		ls.config.set_config({
			history = true,
			updateevents = "TextChanged,TextChangedI",
			delete_check_events = "TextChanged",
			region_check_events = "CursorMoved",
			enable_autosnippets = true,
			ext_opt = {
				[require("luasnip.util.types").choiceNode] = {
					active = {
						virt_text = { { "●", "GruvBoxOrange" } },
					},
				},
			},
		})
		require("luasnip.loaders.from_vscode").lazy_load()
		require("luasnip.loaders.from_lua").load({ paths = "~/.config/nvim/snippets" })

		vim.keymap.set({"i", "s"}, "<C-L>", function() ls.jump( 1) end, {silent = true})
		vim.keymap.set({"i", "s"}, "<C-J>", function() ls.jump(-1) end, {silent = true})

	end
}

