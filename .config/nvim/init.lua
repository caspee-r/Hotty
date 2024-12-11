require("caspeer.options")
require("caspeer.lazy")
require("caspeer.keymaps")
require("caspeer.autocommands")
require("caspeer.functions")
---@diagnostic disable-next-line
vim.cmd([[
try
  colorscheme sonokai
catch /^vim\%((\a\+)\)\=:e185/
  colorscheme default
  set background=dark
endtry
]])

--require("lsp")
--require("caspeer.lua_snip")
--require("caspeer.cmp")
--require("caspeer.treesitter")
--require("caspeer.lualine")
