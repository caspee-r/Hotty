vim.loader.enable()

vim.cmd[[set path+=**]]
vim.cmd[[set path+=~]]
vim.cmd[[set path+=~]]
vim.cmd[[
let g:netrw_keepdir = 0
let g:netrw_winsize = 20
let g:netrw_banner = 0
let g:netrw_list_hide = '\(^\|\s\s\)\zs\.\S\+'
let g:netrw_localcopydircmd = 'cp -r'
hi! link netrwMarkFile Search
let g:netrw_browsex_viewer= "xdg-open"
nnoremap <leader>e :Lexplore %:p:h<CR>
]]
vim.cmd[[let g:markdown_fenced_languages = ["c","lua","python"] ]]

local options = {
	backup = false, -- creates a backup file
	ruler = true,
    wildignorecase = true,
    wildignore = '*.git/*',
	showcmd = true,
	clipboard = "unnamedplus", -- allows neovim to access the system clipboard
	colorcolumn = "80",
	cmdheight = 1, -- more space in the neovim command line for displaying messages
	completeopt = { "menuone", "noselect" }, -- mostly just for cmp conceallevel = 0,                        -- so that `` is visible in markdown files
	fileencoding = "utf-8", -- the encoding written to a file
	hlsearch = false, -- highlight all matches on previous search pattern
	incsearch = true,
	hidden = true,
	visualbell = true,
	ignorecase = true, -- ignore case in search patterns
	showmatch = true,
	-- mouse = "a",                             -- allow the mouse to be used in neovim
	pumheight = 10, -- pop up menu height
	showmode = false, -- we don't need to see things like -- INSERT -- anymore
	showtabline = 2, -- always show tabs
	smartcase = true, -- smart case
	smartindent = true, -- make indenting smarter again
	splitbelow = true, -- force all horizontal splits to go below current window
	splitright = true, -- force all vertical splits to go to the right of current window
	swapfile = false, -- creates a swapfile
	termguicolors = true, -- set term gui colors (most terminals support this)
	timeoutlen = 1000, -- time to wait for a mapped sequence to complete (in milliseconds)
	undofile = true, -- enable persistent undo
	updatetime = 40, -- faster completion (4000ms default)
	writebackup = false, -- if a file is being edited by another program (or was written to file while editing with another program), it is not allowed to be edited
	expandtab = false, -- convert tabs to spaces
	shiftwidth = 4, -- the number of spaces inserted for each indentation
	tabstop = 4, -- insert 2 spaces for a tab
	softtabstop = 4,
	cursorline = true, -- highlight the current line
	number = true, -- set numbered lines
	relativenumber = true, -- set relative numbered lines
	numberwidth = 4, -- set number column width to 2 {default 4}
	foldcolumn = "1",
	foldlevel = 99, -- Using ufo provider need a large value, feel free to decrease the value
	foldlevelstart = 99,
	foldenable = true,
	signcolumn = "yes", -- always show the sign column, otherwise it would shift the text each time
	wrap = false, -- display lines as one long line
	scrolloff = 5, -- is one of my fav
	sidescrolloff = 5,
	guifont = "monospace:h17", -- the font used in graphical neovim applications
	autoindent = true,
	autochdir = true,
	--[[ winbar = '%f', ]]
	compatible = false,
	--[[ iskeyword = '-', ]]
	whichwrap = "<,>,[,],h,l",
}
vim.opt.shortmess:append("c")
for option_name, options_value in pairs(options) do
	vim.opt[option_name] = options_value
end

-- vim.cmd "set whichwrap+=<,>,[,],h,l"
--[[ vim.cmd("set nowritebackup") ]]
--[[ vim.cmd("set nocompatible") ]]
-- vim.cmd [[set iskeyword+=-]]
--
