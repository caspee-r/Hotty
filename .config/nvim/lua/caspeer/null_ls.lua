local cmp_status_ok, null_ls = pcall(require, "null-ls")
if not cmp_status_ok then
    return
end

local formatting = null_ls.builtins.formatting
local diagnostics = null_ls.builtins.diagnostics
local codeactions = null_ls.builtins.code_actions
local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

null_ls.setup {
    on_attach = function(client, bufnr)
        if client.supports_method("textDocument/formatting") then
            vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
            vim.api.nvim_create_autocmd("BufWritePre", {
                group = augroup,
                buffer = bufnr,
                callback = function()
                    -- on 0.8, you should use vim.lsp.buf.format({ bufnr = bufnr }) instead
                    -- on later neovim version, you should use vim.lsp.buf.format({ async = false }) instead
                    vim.lsp.buf.format{async=false}
                end,
            })
        end
    end,

    sources = {
        formatting.black, --.with( {extra_args = {"--fast "}} ),
        formatting.stylua,
        formatting.eslint,
        diagnostics.flake8,
        diagnostics.eslint,
        codeactions.eslint,


    }
}
