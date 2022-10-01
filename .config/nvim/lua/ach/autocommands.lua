-- All my Autocmd
local autocmd = vim.api.nvim_create_autocmd

vim.api.nvim_create_augroup('colorcolumn', { clear = true })
autocmd('BufWinEnter', {
    pattern = '*.py',
    callback = function()
        vim.cmd("highlight ColorColumn ctermbg=red")
        vim.cmd("set colorcolumn=81")
    end
})
vim.cmd(" au TextYankPost * silent! lua vim.highlight.on_yank()")

--vim.api.nvim_create_augroup('',{clear = true})


