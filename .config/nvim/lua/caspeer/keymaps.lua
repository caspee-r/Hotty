local opts = { noremap = true, silent = true }

local term_opts = { silent = true }

-- abbreviation
vim.cmd("ab tel Telescope")

-- Shorten function name
local keymap = vim.api.nvim_set_keymap


--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
keymap("n", "<leader>e", ":Lexplore<CR>", term_opts)

vim.keymap.set("i", "<C-c>", "<Esc>")
vim.keymap.set("n", "<leader><CR>", ":w<CR>")

-- Remap window split
vim.keymap.set("n","<C-w>\\",":vsplit<CR>")
vim.keymap.set("n","<C-w>-",":split<CR>")



-- Most used functions
--vim.keymap.set("n", "<leader>zf", "<cmd>Telekasten find_notes<CR>")
--vim.keymap.set("n", "<leader>zg", "<cmd>Telekasten search_notes<CR>")
--vim.keymap.set("n", "<leader>zd", "<cmd>Telekasten goto_today<CR>")
--vim.keymap.set("n", "<leader>zz", "<cmd>Telekasten follow_link<CR>")
--vim.keymap.set("n", "<leader>zn", "<cmd>Telekasten new_note<CR>")
--vim.keymap.set("n", "<leader>zc", "<cmd>Telekasten show_calendar<CR>")
--vim.keymap.set("n", "<leader>zb", "<cmd>Telekasten show_backlinks<CR>")
--vim.keymap.set("n", "<leader>zI", "<cmd>Telekasten insert_img_link<CR>")

-- Call insert link automatically when we start typing a link

-- a template for text objects
--vim.cmd [[
--xnoremap <silent> i, :<c-u>normal! T,vt,<cr>
--onoremap <silent> i, :<c-u>normal! T,vt,<cr>
--xnoremap <silent> a, :<c-u>normal! F,ft,<cr>
--onoremap <silent> a, :<c-u>normal! F,ft,<cr>
--]]
--
--[[ map <C-H> <Nop> ]]
--[[ ]]
-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Normal --
-- Better window navigation
--[[ keymap("n", "<C-h>", "<C-w>h", opts) ]]
--[[ keymap("n", "<C-j>", "<C-w>j", opts) ]]
--[[ keymap("n", "<C-k>", "<C-w>k", opts) ]]
--[[ keymap("n", "<C-l>", "<C-w>l", opts) ]]

-- execute the last commands
--vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)

-- greatest remap ever
vim.keymap.set("x", "<leader>p", [["_dP]])
-- next greatest remap ever : asbjornHaland
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])
vim.keymap.set({ "n", "v" }, "<leader>d", [["_d]])
vim.keymap.set("n", "<leader>x", "<cmd>!chmod u+x %<CR>", { silent = true })

-- source file
vim.keymap.set("n", "<leader><leader>s","<cmd>source %<CR>")
-- source line
vim.keymap.set("n","<leader>s",":.lua<CR>")
-- source selection
vim.keymap.set("v","<leader>s",":lua<CR>")

-- Resize with arrows
keymap("n", "<C-Up>", ":resize +2<CR>", opts)
keymap("n", "<C-Down>", ":resize -2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)

--
keymap("n", "<C-d>", "<C-d>zz", opts)
keymap("n", "<C-u>", "<C-u>zz", opts)


-- buffer cycling
keymap("n", "<A-l>", ":bn<CR>", opts)
keymap("n", "<A-h>", ":bp<CR>", opts)



--

--vim.cmd [[nmap <S-Enter> O<Esc>j
--nmap <CR> o<Esc>k
--]]
vim.cmd [[
noremap <leader>; q:is:::g<Left><Left><Left>
noremap <leader>% q:i%s:::g<Left><Left><Left>
noremap <leader>" q:i%s:::cg<Left><Left><Left><Left>
]]

--vim.cmd [[
--noremap : q:i
--]]

-- vim paste whate you yank not delete
keymap("n", ",p", '"0p', {})
keymap("n", ",P", '"0P', {})

-- alternate file
keymap("n", "<leader>m", ":e #<CR>", opts)

-- quickfix list
keymap("n", "<leader>co", ":copen<CR>", opts)
keymap("n", "<leader>cc", ":cclose<CR>", opts)
keymap("n", "[c", ":cprev<CR>", opts)
keymap("n", "]c", ":cnext<CR>", opts)


-- lsp formatting
local Caspeer = vim.api.nvim_create_augroup('CaspeerLSP',{ })
vim.api.nvim_create_autocmd('LspAttach',
{
	group = Caspeer,
	callback = function(e)
		local optx = {buffer = e.buf }
		vim.keymap.set("n", "<leader>f", vim.lsp.buf.format,optx)
		vim.keymap.set("n", "<leader>r", vim.lsp.buf.references,optx)
		vim.keymap.set("n", "gd", vim.lsp.buf.definition,optx)
		vim.keymap.set("n", "K", vim.lsp.buf.hover,optx)
		vim.keymap.set("n", "gi", vim.lsp.buf.implementation,optx)
		vim.keymap.set("n","<leader>k",vim.lsp.buf.signature_help,optx)
		vim.keymap.set("n","<leader>rn",vim.lsp.buf.rename,optx)
		vim.keymap.set("n","<leader>rf",vim.lsp.buf.references,optx)
		vim.keymap.set("n","<leader>ca",vim.lsp.buf.code_action,optx)
		vim.keymap.set("n","<leader>of",vim.diagnostic.open_float,optx)
		vim.keymap.set("n","[e",vim.diagnostic.goto_next,optx)
		vim.keymap.set("n","]e",vim.diagnostic.goto_prev,optx)
	end
	}
	)
--	-- Navigate buffers

-- Move text up and down
keymap("n", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)
keymap("n", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)

-- Telescope
keymap("n", "<leader>ff", "<cmd>Telescope find_files<CR>", opts)
keymap("n", "<leader>fg", "<cmd>Telescope live_grep<CR>", opts)
keymap("n", "<leader>fb", "<cmd>Telescope buffers<CR>", opts)
keymap("n", "<leader>fc", "<cmd>Telescope current_buffer_fuzzy_find<CR>", opts)
keymap("n", "<leader>fs", "<cmd>Telescope grep_string<CR>", opts)
keymap("n", "<leader>fo", "<cmd>Telescope oldfiles<CR>", opts)

-- Visual --
--  Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

keymap("v", "p", '"_dP', opts)

-- Visual Block --
-- Move text up and down
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
--keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)
--keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)

-- Better terminal navigation
keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)


keymap("n","<leader>i",":make<CR>",opts)

