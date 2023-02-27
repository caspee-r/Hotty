require("ach.options")
require("ach.lazy")
require("lsp")
vim.cmd([[
try
  colorscheme kanagawa
catch /^Vim\%((\a\+)\)\=:E185/
  colorscheme default
  set background=dark
endtry
]])

require("ach.keymaps")
require("ach.lua_snip")
require("ach.cmp")
require("ach.autocommands")
--[[ require "ach.telescope" ]]
require("ach.autopairs")
require("ach.comment")
require("ach.bufferline")
require("ach.treesitter")
require("ach.lualine")
require("ach.gitsigns")
--[[ require "ach.neosolarized" ]]
----[[ require "ach.trouble" ]]
require("ach.null_ls")
require("ach.neoscroll")
----[[ require "hop" ]]
--[[ require "ach.nvim-tree" ]]
require("ach.blankline")
require("ach.impatient")
----[[ require "project" ]]
----[[ require "md-previwer" ]]
----[[ require "functions" ]]
