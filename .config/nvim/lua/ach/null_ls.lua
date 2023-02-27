local cmp_status_ok, null_ls = pcall(require, "null-ls")
if not cmp_status_ok then
    return
end

local formatting = null_ls.builtins.formatting
local diagnostics = null_ls.builtins.diagnostics
local codeactions = null_ls.builtins.code_actions
null_ls.setup {


    sources = {
        formatting.black,--.with( {extra_args = {"--fast "}} ),
        formatting.stylua,
        formatting.eslint,
        diagnostics.flake8,
        diagnostics.eslint,
        codeactions.eslint,


    }
}
