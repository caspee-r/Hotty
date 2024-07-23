require("caspeer.options")
require("caspeer.lazy")
---@diagnostic disable-next-line
vim.cmd([[
try
  colorscheme kanagawa
catch /^vim\%((\a\+)\)\=:e185/
  colorscheme default
  set background=dark
endtry
]])
require("caspeer.keymaps")
require("caspeer.autocommands")
require("caspeer.functions")
--require("lsp")
--require("caspeer.lua_snip")
--require("caspeer.cmp")
--require("caspeer.treesitter")
--require("caspeer.lualine")
