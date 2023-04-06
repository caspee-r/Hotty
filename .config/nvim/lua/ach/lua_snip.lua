local ls = require("luasnip")


ls.config.set_config({
    history=true,
    updateevents="TextChanged,TextChangedI",
    enable_autosnippets = true,
    ext_opt={
        [require("luasnip.util.types").choiceNode] = {
            active = {
                virt_text = {{"●","GruvBoxOrange"}}
            }
        }
    }

})

require("luasnip.loaders.from_lua").load({paths="~/.config/nvim/snippets"})
--[[ vim.api.nvim_set_keymap("i", "<C-n>", "<Plug>luasnip-next-choice", {}) ]]
--[[ vim.api.nvim_set_keymap("s", "<C-n>", "<Plug>luasnip-next-choice", {}) ]]
--[[ vim.api.nvim_set_keymap("i", "<C-p>", "<Plug>luasnip-prev-choice", {}) ]]
--[[ vim.api.nvim_set_keymap("s", "<C-p>", "<Plug>luasnip-prev-choice", {}) ]]
