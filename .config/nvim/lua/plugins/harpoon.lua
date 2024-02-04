return {
    "ThePrimeagen/harpoon",
    opts = {},
    keys = {
        {"<leader>h",':lua require("harpoon.ui").toggle_quick_menu()<CR>',silent = true},
        {"<leader>'",':lua require("harpoon.mark").add_file()<CR>',silent = true},
        {"<A-1>",':lua require("harpoon.ui").nav_file(1)<CR>',silent = true},
        {"<A-2>",':lua require("harpoon.ui").nav_file(2)<CR>',silent = true},
        {"<A-3>",':lua require("harpoon.ui").nav_file(3)<CR>',silent = true},
        {"<A-4>",':lua require("harpoon.ui").nav_file(4)<CR>',silent = true},
    }
}

