

return {
    'rebelot/heirline.nvim',
    opts = function(_, opts)
        vim.opt.fillchars:append { eob = ' ', stl = '─', stlnc = '─' }
        local conditions = require 'heirline.conditions'
        local utils = require 'heirline.utils'
		local function setup_colors()
			return {
				bright_bg = utils.get_highlight("Folded").bg,
				bright_fg = utils.get_highlight("Folded").fg,
				red = utils.get_highlight("DiagnosticError").fg,
				dark_red = utils.get_highlight("DiffDelete").bg,
				green = utils.get_highlight("String").fg,
				blue = utils.get_highlight("Function").fg,
				gray = utils.get_highlight("NonText").fg,
				orange = utils.get_highlight("Constant").fg,
				purple = utils.get_highlight("Statement").fg,
				cyan = utils.get_highlight("Special").fg,
				diag_warn = utils.get_highlight("DiagnosticWarn").fg,
				diag_error = utils.get_highlight("DiagnosticError").fg,
				diag_hint = utils.get_highlight("DiagnosticHint").fg,
				diag_info = utils.get_highlight("DiagnosticInfo").fg,
				git_del = utils.get_highlight("GitSignsDelete").fg,
				git_add = utils.get_highlight("GitSignsAdd").fg,
				git_change = utils.get_highlight("GitSignsChange").fg,
			}
		end

		-- require("heirline").load_colors(setup_colors)
		-- or pass it to config.opts.colors

		vim.api.nvim_create_augroup("Heirline", { clear = true })
		vim.api.nvim_create_autocmd("ColorScheme", {
			callback = function()
				utils.on_colorscheme(setup_colors)
			end,
			group = "Heirline",
		})
		local Align = { provider = '%=' }
		local Space = { provider = '───', hl = 'Normal' }
		local pwd = {
			init = function (self)
				self.pwd = vim.fn.fnamemodify(vim.fn.getcwd(0),':~')
			end,
			hl = {fg='red',bold = true},
			flexible = 1;
			{
				-- evaluates to the full-lenth path
				provider = function(self)
					local trail = self.pwd:sub(-1) == "/" and "" or "/"
					return self.pwd .. trail .." "
				end,
			},
			{
				-- evaluates to the shortened path
				provider = function(self)
					local pwd = vim.fn.pathshorten(self.pwd)
					local trail = self.pwd:sub(-1) == "/" and "" or "/"
					return  pwd .. trail .. " "
				end,
			},

		}
		local Git = {
			condition = conditions.is_git_repo,
			init = function(self)
				self.status_dict = vim.b.gitsigns_status_dict
				self.has_changes = self.status_dict.added ~= 0
				or self.status_dict.removed ~= 0
				or self.status_dict.changed ~= 0
			end,
			hl = { fg = 'orange' },
			Space,
			{ -- git branch name
				provider = function(self)
					return ' ' .. self.status_dict.head
				end,
				hl = { bold = true },
			},

			{
				condition = function(self)
					return self.has_changes
				end,
				provider = "["
			},
			{
				provider = function(self)
					local count = self.status_dict.added or 0
					return count > 0 and (' +' .. count)
				end,
				hl = {fg = "git_add"},
			},
			{
				provider = function(self)
					local count = self.status_dict.removed or 0
					return count > 0 and (' -' .. count)
				end,
				hl = { fg = "git_del" },
			},
			{
				provider = function(self)
					local count = self.status_dict.changed or 0
					return count > 0 and (' ~' .. count)
				end,
				hl = { fg = "git_change" },
			},
			{
				condition = function(self)
					return self.has_changes
				end,
				provider = "]",
			},
		}
		local FileFormat = {
			provider = function()
				local enc = (vim.bo.fenc ~= '' and vim.bo.fenc) or vim.o.enc
				local fmt = vim.bo.fileformat
				return ' ' .. enc .. '[' .. fmt .. '] '
			end,
			hl = 'Normal',
		}
		local Ruler = {
			--provider = ' %l/%L | %c/%-2{virtcol("$") - 1} ',
			provider = ' [ %l : %c ]',
			hl = { fg = 'red' },
		}
		local Modified = {
			init = function (self)
				self.modified = vim.bo.modified
			end,
			hl = {fg="bright_fg"},
			{
				provider = function	()
					return "["
				end
			},

			{
				provider = function	(self)
					if self.modified then
						return "+"
					end
				end,
				hl = { fg =  'blue', bold = true },
			},
			{
				provider = function	()
					return "]"
				end,
			},


		}
		-- statusline
		local DefaultStatusline = {
			Git, Space, pwd, Align,
			Modified, Ruler, Space,
		}
		local InactiveStatusline = {
			condition = conditions.is_not_active,
			Align,
		}
		local Help = {

			provider = function()
				local filename = vim.api.nvim_buf_get_name(0)
				return vim.fn.fnamemodify(filename, ":t")
			end,
			hl = { fg = "blue" ,bold = true },
		}
		local HelpStatusLine = {
			condition = function ()
				return conditions.buffer_matches({
					buftype = {'help'},
				})
			end,
			Align,Help,Align
		}
		local SpecialStatusline = {
			condition = function()
				return conditions.buffer_matches({
					buftype = { 'nofile', 'prompt', 'quickfix'},
					filetype = { '^git.*', 'fugitive' ,'netrw' },
				})
			end,
			Align,
		}
		opts.statusline = {
			fallthrough = false,
			SpecialStatusline,
			HelpStatusLine,
			InactiveStatusline,
			DefaultStatusline,
		}
		-- tabline
		local TablineFileName = {
			provider = function(self)
				local filename = self.filename
				filename = filename == '' and '[No Name]' or vim.fn.fnamemodify(filename, ':t')
				return filename
			end,
		}
		local TablineFileNameBlock = {
			init = function(self)
				self.filename = vim.api.nvim_buf_get_name(self.bufnr)
			end,
			hl = function(self)
				if self.is_active then
					return 'TabLineSel'
				else
					return 'TabLine'
				end
			end,
			{ provider = ' ' },
			TablineFileName,
			{ provider = ' ' },
		}
		local BufferLine = utils.make_buflist(TablineFileNameBlock)
		local Tabpage = {
			provider = function(self)
				return '%' .. self.tabnr .. 'T ' .. self.tabpage .. ' %T'
			end,
			hl = function(self)
				if not self.is_active then
					return 'TabLineSel'
				else
					return 'TabLine'
				end
			end,
		}
		local TabPages = {
			condition = function()
				return #vim.api.nvim_list_tabpages() >= 2
			end,
			utils.make_tablist(Tabpage),
		}
		opts.tabline = { BufferLine, Align, TabPages }
	end,
}
