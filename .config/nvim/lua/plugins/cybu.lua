return {
    "ghillb/cybu.nvim",
    branch = "main", -- timely updates
    --dependencies = { "nvim-tree/nvim-web-devicons", "nvim-lua/plenary.nvim"}, -- optional for icon support
    config = function()
        local ok, cybu = pcall(require, "cybu")
        if not ok then
            return
        end
        cybu.setup()
        vim.keymap.set("n", "[b", "<Plug>(CybuPrev)")
        vim.keymap.set("n", "]b", "<Plug>(CybuNext)")
        vim.keymap.set({"n", "v"}, "<s-tab>", "<plug>(CybuLastusedPrev)")
        vim.keymap.set({"n", "v"}, "<tab>", "<plug>(CybuLastusedNext)")
    end,
}
