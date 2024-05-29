-- All my Autocmd
local autocmd = vim.api.nvim_create_autocmd
--local Caspeer = vim.api.nvim_create_augroup

vim.api.nvim_create_augroup("colorcolumn", { clear = true })
vim.api.nvim_create_augroup("helpfiles",{clear = true})

vim.cmd [[
augroup templates
autocmd BufNewFile *.c 0r ~/.config/nvim/templates/c.skel
]]
vim.cmd(" au TextYankPost * silent! lua vim.highlight.on_yank()")

vim.cmd [[
autocmd FileType help nnoremap <buffer> <CR> <C-]>
autocmd FileType help nnoremap <buffer> <BS> <C-T>
autocmd FileType help nnoremap <buffer> o /'\l\{2,\}'<CR>
autocmd FileType help nnoremap <buffer> O ?'\l\{2,\}'<CR>
autocmd FileType help nnoremap <buffer> s /\|\zs\S\+\ze\|<CR>
autocmd FileType help nnoremap <buffer> S ?\|\zs\S\+\ze\|<CR>
]]

vim.cmd [[
augroup locallist
    autocmd!
    " Populate locallist with lsp diagnostics automatically 
    autocmd User LspDiagnosticsChanged :lua vim.lsp.diagnostic.set_loclist({open_loclist = false})
augroup END
]]


-- Delete [No Name] buffers
autocmd("BufHidden", {
  desc = "Delete [No Name] buffers",
  callback = function(event)
    if event.file == "" and vim.bo[event.buf].buftype == "" and not vim.bo[event.buf].modified then
      vim.schedule(function() pcall(vim.api.nvim_buf_delete, event.buf, {}) end)
    end
  end,
})

autocmd("TermOpen", {
  desc = "drop in the term in insert mode",
  callback = function(event)
	  vim.bo.modifiable = true
	  vim.api.nvim_input("i")

  end,
})
