local servers = {
	"pyright",
	"sumneko_lua",
	"rust_analyzer",
	"html",
	"tailwindcss",
	"cssls",
	"eslint",
	"tsserver",
	"emmet_ls",
	"ccls",
    "bashls"
}

require("nvim-lsp-installer").setup({
	ensure_installed = servers,
})

for _, server in pairs(servers) do
	local opts = {
		on_attach = require("ach.lsp.handlers").on_attach,
		capabilities = require("ach.lsp.handlers").capabilities,
	}
	local has_custom_opts, server_custom_opts = pcall(require, "ach.lsp.settings." .. server)
	if has_custom_opts then
		opts = vim.tbl_deep_extend("force", server_custom_opts, opts)
		require("lspconfig")[server].setup(opts)
	end
end
