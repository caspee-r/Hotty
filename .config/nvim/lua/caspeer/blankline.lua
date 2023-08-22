local status_ok, indent = pcall(require, "indent_blankline")
if not status_ok then
	return
end

vim.cmd([[highlight IndentBlanklineIndent1 guifg=#202021 gui=nocombine]])
--vim.cmd([[highlight IndentBlanklineIndent2 guifg=#E5C07B gui=nocombine]])
--vim.cmd([[highlight IndentBlanklineIndent3 guifg=#98C379 gui=nocombine]])
--vim.cmd([[highlight IndentBlanklineIndent4 guifg=#56B6C2 gui=nocombine]])
--vim.cmd([[highlight IndentBlanklineIndent5 guifg=#61AFEF gui=nocombine]])
--vim.cmd([[highlight IndentBlanklineIndent6 guifg=#C678DD gui=nocombine]])
--vim.cmd([[highlight IndentBlanklineChar guifg=#C678DD gui=nocombine]])
--vim.cmd([[highlight IndentBlanklineSpaceChar guifg=#C678DD gui=nocombine]])

vim.cmd([[let g:indent_blankline_char='┆']])

vim.opt.list = true
vim.opt.listchars:append("eol:↴")

indent.setup({
	show_end_of_line = true,
	char_highlight_list = {
		"IndentBlanklineIndent1",
	},
})
