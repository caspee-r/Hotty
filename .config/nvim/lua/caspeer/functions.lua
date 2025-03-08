
local fn = vim.fn
local api = vim.api

local function map(m, l, r)
	vim.keymap.set(m, l, r, { noremap = true, silent = true })
end

vim.api.nvim_set_keymap("n", "<leader>cp", [[:let @+ = expand('%:p')<CR>]], { noremap = true, silent = true })
function copy_buffer_file_path(bufnr)
	local filepath = vim.api.nvim_buf_get_name(0)
	vim.fn.setreg("+", filepath) -- Set the clipboard register
end

function make()
	local make_program = fn.input("Command: ",vim.bo.makeprg)
	vim.bo.makeprg = make_program
	vim.cmd("make")
end

map({ 'n', 'x' }, "<leader>o", make)   -- next equal indent

function indent_traverse(direction, equal) -- {{{
return function()
	-- Get the current cursor position
	local current_line, column = unpack(api.nvim_win_get_cursor(0))
	local match_line = current_line
	local match_indent = false
	local match = false

	local buf_length = api.nvim_buf_line_count(0)

	-- Look for a line of appropriate indent
	-- level without going out of the buffer
	while (not match)
		and (match_line ~= buf_length)
		and (match_line ~= 1)
		do
			match_line = match_line + direction
			local match_line_str = api.nvim_buf_get_lines(0, match_line - 1, match_line, false)[1]
			-- local match_line_is_whitespace = match_line_str and match_line_str:match('^%s*$')
			local match_line_is_whitespace = match_line_str:match('^%s*$')

			if equal then
				match_indent = fn.indent(match_line) <= fn.indent(current_line)
			else
				match_indent = fn.indent(match_line) < fn.indent(current_line)
			end
			match = match_indent and not match_line_is_whitespace
		end

		-- If a line is found go to line
		if match or match_line == buf_length then
			fn.cursor({ match_line, column + 1 })
		end
	end
end                                                 -- }}}
map({ 'n', 'x' }, "gj", indent_traverse(1, true))   -- next equal indent
map({ 'n', 'x' }, 'gk', indent_traverse(-1, true))  -- previous equal indent
map({ 'n', 'x' }, 'gJ', indent_traverse(1, false))  -- next equal indent
map({ 'n', 'x' }, 'gK', indent_traverse(-1, false)) -- previous equal indent

function CloseAllExceptCurrent()
	local current_buf = api.nvim_get_current_buf()
	local all_bufs = api.nvim_list_bufs()

	for _, buf in ipairs(all_bufs) do
		if buf ~= current_buf then
			api.nvim_buf_delete(buf, { force = true })
		end
	end
end
map({"n"},"<leader>bx",":lua CloseAllExceptCurrent()<CR>")
