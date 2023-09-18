return {
--    "nvim-tree/nvim-tree.lua",
    config = function ()

        local status_ok, nvim_tree = pcall(require, "nvim-tree")
        if not status_ok then
            return
        end

        local config_status_ok, nvim_tree_config = pcall(require, "nvim-tree.config")
        if not config_status_ok then
            return
        end
        local tree_cb = nvim_tree_config.nvim_tree_callback
        vim.g.loaded_netrw = 1
        vim.g.loaded_netrwPlugin = 1

        -- set termguicolors to enable highlight groups
        vim.opt.termguicolors = true

        nvim_tree.setup {
            disable_netrw = false,
            hijack_netrw = false,
            hijack_cursor = false,
            update_cwd = true,
            respect_buf_cwd = true,
            sort_by = "case_sensitive",
            diagnostics = {
                enable = true,
                icons = {
                    hint = "",
                    info = "",
                    warning = "",
                    error = "",
                },
            },
            update_focused_file = {
                enable = true,
                update_cwd = false,
                ignore_list = {},
            },
            system_open = {
                cmd =nil,
                args = {},
            },
            filters = {
                dotfiles = true,
                custom = {},
            },
            git = {
                enable = true,
                ignore = true,
                timeout = 500,
            },
            view = {
                width = 20,
                --[[ height = 30, ]]
                hide_root_folder = false,
                side = "left",
                mappings = {
                    custom_only = false,
                    list = {
                        { key = { "l", "<CR>", "o" }, cb = tree_cb "edit" },
                        { key = "h", cb = tree_cb "close_node" },
                        { key = "v", cb = tree_cb "vsplit" },
                        { key = "m", cb = tree_cb "create dir" },

                    },
                },
                number = false,
                relativenumber = false,
            },

            trash = {
                cmd = "trash",
                require_confirm = true,
            },
            renderer = {
                icons = {
                    glyphs = {

                        default = "",
                        symlink = "",
                        git = {
                            unstaged = "",
                            staged = "S",
                            unmerged = "",
                            renamed = "➜",
                            deleted = "",
                            untracked = "U",
                            ignored = "◌",
                        },
                        folder = {
                            default = "",
                            open = "",
                            empty = "",
                            empty_open = "",
                            symlink = "",
                        },
                    }
                } ,
                indent_markers = {
                    enable = true,
                    icons = {
                        corner = "└ ",
                        edge = "│ ",
                        none = "  ",
                    }
                },
            }
        }
        end,
        tag = "nightly",
        keys = {
            {"<leader>e","<cmd>:lua require('nvim-tree.api').tree.toggle()<cr>",desc="toggle NVIM-TREE"}
        }
    }


