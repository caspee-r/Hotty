-- All my Autocmd
local autocmd = vim.api.nvim_create_autocmd

vim.api.nvim_create_augroup('colorcolumn', { clear = true })

local function open_nvim_tree(data)
    local no_name = data.file == "" and vim.bo[data.buf].buftype == ""
    local dir_name = vim.fn.isdirectory(data.file) == 1
    if not no_name   then
        if not dir_name then
            return
        else
            vim.cmd.cd(data.file)
            require("nvim-tree.api").tree.open({focus=true,find_file=true})
        end
    else
        require("nvim-tree.api").tree.toggle({focus=false,find_file=true})
    end
end

--[[ vim.api.nvim_create_autocmd({ "BufWritePost" },{pattern="*.tex","*.md", ]]
--[[ command="lua req"}) ]]
vim.cmd(" au TextYankPost * silent! lua vim.highlight.on_yank()")
autocmd(
    {'VimEnter'},
    {callback = open_nvim_tree}
)

--[[ autocmd( ]]
--[[ {"BufNewFile"}, ]]
--[[ {pattern = "*.py", ]]
--[[     command = "read ~/tst.py"} ]]
--[[ ) ]]


