-------------------- Configs -------------

local servers = {
	"pyright",
	--[[ "rnix"; ]]
	"lua_ls",
	"rust_analyzer",
	"html",
	"tailwindcss",
	"cssls",
	"eslint",
	"tsserver",
	"emmet_ls",
	"clangd",
	"bashls",
	"texlab",
	"html",
	"yamlls",
	"nil_ls",
}

require("mason").setup({
	ensure_installed = servers,
})

local M = require("lsp.handlers")

for _, server in pairs(servers) do
	local opts = {
		on_attach = M.on_attach,
		capabilities = M.capabilities,
	}
	local has_custom_opts, server_custom_opts = pcall(require, "lsp.settings." .. server)
	if has_custom_opts then
		opts = vim.tbl_deep_extend("force", server_custom_opts, opts)
    require("lspconfig")[server].setup(opts)
	end
end
