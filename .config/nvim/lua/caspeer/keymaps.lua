local opts = { noremap = true, silent = true }

local term_opts = { silent = true }


-- abbreviation
vim.cmd("ab tel Telescope")
vim.cmd("ab sm SessionManager")

-- Shorten function name
local keymap = vim.api.nvim_set_keymap

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)
keymap("n", "<leader>e", ":Lexplore<CR>", term_opts)

vim.keymap.set("i", "<C-c>", "<Esc>")




-- a template for text objects
--vim.cmd [[
--xnoremap <silent> i, :<c-u>normal! T,vt,<cr>
--onoremap <silent> i, :<c-u>normal! T,vt,<cr>
--xnoremap <silent> a, :<c-u>normal! F,ft,<cr>
--onoremap <silent> a, :<c-u>normal! F,ft,<cr>
--]]
--
--[[ vim.cmd [[ ]]
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
vim.keymap.set("n", "<leader>pv", vim.cmd.Ex)

-- greatest remap ever
vim.keymap.set("x", "<leader>p", [["_dP]])
-- next greatest remap ever : asbjornHaland
vim.keymap.set({ "n", "v" }, "<leader>y", [["+y]])
vim.keymap.set("n", "<leader>Y", [["+Y]])
vim.keymap.set({ "n", "v" }, "<leader>d", [["_d]])
vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })

-- source file
vim.keymap.set("n", "<leader><leader>", function()
    vim.cmd("so")
end)
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
vim.keymap.set("n", "<leader>f", vim.lsp.buf.format)
vim.keymap.set("n", "<leader>r", vim.lsp.buf.references)
-- Navigate buffers

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

--Trouble--
keymap("n", "<leader>q", "<cmd>TroubleToggle<CR>", opts)

vim.cmd [[
function! ZoteroCite()
  let format = &filetype =~ '.*tex' ? 'citep' : 'pandoc'
  let api_call = 'http://127.0.0.1:23119/better-bibtex/cayw?format='.format.'&brackets=1'
  let ref = system('curl -s '.shellescape(api_call))
  return ref
endfunction

noremap <leader>tz "=ZoteroCite()<CR>p
inoremap <C-z> <C-r>=ZoteroCite()<CR>

]]
