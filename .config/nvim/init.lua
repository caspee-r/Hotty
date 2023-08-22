require("caspeer.options")
require("caspeer.lazy")
vim.cmd([[
try
  colorscheme kanagawa
catch /^Vim\%((\a\+)\)\=:E185/
  colorscheme default
  set background=dark
endtry
]])
require("caspeer.autocommands")
require("lsp")
require("caspeer.keymaps")
require("caspeer.lua_snip")
require("caspeer.cmp")
--[[ require "caspeer.telescope" ]]
require("caspeer.autopairs")
require("caspeer.comment")
require("caspeer.bufferline")
require("caspeer.treesitter")
require("caspeer.lualine")
require("caspeer.gitsigns")
require("caspeer.blankline")
