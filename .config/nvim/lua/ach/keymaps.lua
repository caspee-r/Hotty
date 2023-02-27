local opts = { noremap = true, silent = true }

local term_opts = { silent = true }

-- abbreviation
vim.cmd("ab tel Telescope")
vim.cmd("ab sm SessionManager")


-- Shorten function name
local keymap = vim.api.nvim_set_keymap

--Remap space as leader key
keymap("", "<Space>", "<Nop>", opts)

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

-- Resize with arrows
keymap("n", "<C-Up>", ":resize +2<CR>", opts)
keymap("n", "<C-Down>", ":resize -2<CR>", opts)
keymap("n", "<C-Left>", ":vertical resize -2<CR>", opts)
keymap("n", "<C-Right>", ":vertical resize +2<CR>", opts)

-- split vertically and show the next buffer
--[[ keymap("n", "<C-V>", ":vsplit ") ]]

-- switch to zen mode
keymap("n", "<leader>z",":ZenMode<CR>",opts)

-- vim paste whate you yank not delete
keymap("n", ",p", "\"0p", {})
keymap("n", ",P", "\"0P", {})

-- telescope abbr
--[[ keymap("n", "tl", ":Telescope<CR>", opts) ]]

-- lsp formatting
keymap("n", "<leader>f", ":Format<CR>", opts)
-- Navigate buffers

-- Move text up and down
keymap("n", "<A-j>", "<Esc>:m .+1<CR>==gi", opts)
keymap("n", "<A-k>", "<Esc>:m .-2<CR>==gi", opts)

-- Telescope
--[[ keymap("n", "<leader>ff", "<cmd>Telescope find_files<CR>", opts) ]]
--[[ keymap("n", "<leader>fg", "<cmd>Telescope live_grep<CR>", opts) ]]
--[[ keymap("n", "<leader>fb", "<cmd>Telescope buffers<CR>", opts) ]]
--[[ keymap("n", "<leader>fc", "<cmd>Telescope current_buffer_fuzzy_find<CR>", opts) ]]
--[[ keymap("n", "<leader>fs", "<cmd>Telescope grep_string<CR>", opts) ]]
--[[ keymap("n", "<leader>fo", "<cmd>Telescope oldfiles<CR>", opts) ]]

-- Bufferline
    -- cycling
keymap("n", "<A-l>", ":BufferLineCycleNext<CR>", opts)
keymap("n", "<A-h>", ":BufferLineCyclePrev<CR>", opts)
    -- go to buffer
keymap("n","g1",":BufferLineGoToBuffer 1<CR>", opts)
keymap("n","g2",":BufferLineGoToBuffer 2<CR>", opts)
keymap("n","g3",":BufferLineGoToBuffer 3<CR>", opts)
keymap("n","g4",":BufferLineGoToBuffer 4<CR>", opts)
keymap("n","g5",":BufferLineGoToBuffer 5<CR>", opts)
keymap("n","g6",":BufferLineGoToBuffer 6<CR>", opts)
    -- pick buffer
keymap("n","<leader>bc",":BufferLinePickClose<CR>", opts)
    -- toggle pin buffer
keymap("n","<leader>bp",":BufferLineTogglePin<CR>", opts)
keymap("n","<leader>br",":BufferLineCloseRight<CR>", opts)
keymap("n","<leader>bl",":BufferLineCloseLeft<CR>", opts)

-- Neo-tree
--[[ keymap("n", "<leader>e", "<cmd>NvimTreeToggle<CR>", opts) ]]
-- Visual --
--  Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- Move text up and down
keymap("v", "<A-j>", ":m .+1<CR>==", opts)
keymap("v", "<A-k>", ":m .-2<CR>==", opts)
keymap("v", "p", '"_dP', opts)

-- Visual Block --
-- Move text up and down
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)

-- Command
keymap("c", ":SessionManager load_last_session<CR>", ":SessionManager<CR>", { silent = false })
-- Terminal --
-- Better terminal navigation
keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)

--Trouble--
keymap("n","<leader>q","<cmd>TroubleToggle<CR>",opts)

-- Knap
-- set shorter name for keymap function
