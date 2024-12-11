return {
	"hrsh7th/nvim-cmp",
	dependencies = {
		"hrsh7th/cmp-nvim-lsp",
		"hrsh7th/cmp-buffer",
		"hrsh7th/cmp-path",
		"hrsh7th/cmp-cmdline",
		"saadparwaiz1/cmp_luasnip",
	},
	config = function ()

		local kind_icons = {
			Text = "󰊄",
			Method = "m",
			Function = "󰊕",
			Constructor = "",
			Field = "",
			Variable = "",
			Class = "",
			Interface = "",
			Module = "",
			Property = "",
			Unit = "",
			Value = "",
			Enum = "",
			Keyword = "",
			Snippet = "",
			Color = "",
			File = "󰈔",
			Reference = "",
			Folder = "",
			EnumMember = "",
			Constant = "󰏿",
			Struct = "",
			Event = "",
			Operator = "",
			TypeParameter = "",
		}
		local cmp = require "cmp"
		cmp.setup({
			snippet = {
				expand = function(args)
					require("luasnip").lsp_expand(args.body) -- For `luasnip` users.
				end,
			},
			mapping = cmp.mapping.preset.insert({
				['<C-k>'] = cmp.mapping.select_prev_item(cmp_select),
				['<C-j>'] = cmp.mapping.select_next_item(cmp_select),
				["<C-f>"] = cmp.mapping.scroll_docs(4),
				["<C-b>"] = cmp.mapping.scroll_docs(-4),
				--["<C-d>"] = cmp.open_docs(),
				["<Return>"] = cmp.mapping.confirm({ select = true }),
				["<C-Space>"] = cmp.mapping.complete(),
			}),

			view = {
				docs = {
					auto_open = false,
				}
			},
			formatting = {
				fields = { "kind", "abbr", "menu" },
				format = function(entry, vim_item)
					-- Kind icons
					vim_item.kind = string.format("%s ", kind_icons[vim_item.kind])
					-- vim_item.kind = string.format('%s %s', kind_icons[vim_item.kind], vim_item.kind) -- This concatonates the icons with the name of the item kind
					return vim_item
				end,
				vim.cmd,
			},

			sources = cmp.config.sources({
				{ name = 'nvim_lsp' },
				{ name = 'luasnip' }, -- For luasnip users.
				{ name = 'path' },
			}, {
					{ name = 'buffer' },
				}),

			confirm_opts = {
				behavior = cmp.ConfirmBehavior.Replace,
				select = false,
			},
			experimental = {
				ghost_text = true,
				native_menu = false,
			},
		})

--		cmp.setup.cmdline({ '/', '?' }, {
--			mapping = {
--				["<C-j>"] = { c = cmp.mapping.select_next_item() },
--				["<C-k>"] = { c = cmp.mapping.select_prev_item() },
--			},
--			sources = {
--				{ name = 'buffer' }
--			}
--		})
--
--		-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
--		cmp.setup.cmdline(':', {
--			mapping = cmp.mapping.preset.cmdline(),
--			sources = cmp.config.sources({
--				{ name = 'path' }
--			}, {
--					{
--						name = 'cmdline',
--						options = {
--							ignore_cmds = { 'Man', '!' }
--						}
--					}
--				}),
--			matching = { disallow_symbol_nonprefix_matching = false },
--		})
--
	end,
}
