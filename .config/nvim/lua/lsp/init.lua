-------------------- Configs -------------

local servers = {
    "pyright",
    --[[ "rnix"; ]]
    "lua_ls",
    "rust_analyzer",
    "bashls",
    "tsserver",
    "clangd",
}

require("mason").setup({
    ui = {
        keymaps = {
            apply_language_filter = "<C-g>",
        },
    },
})

require("mason-lspconfig").setup({
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
