local ls = require("luasnip")

require("luasnip.loaders.from_lua").load({path="~/.config/nvim/snippets"})

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
