local download = {
    -- EDITOR ENLIGHTENMENT
    "nvim-treesitter/nvim-treesitter",
    "neovim/nvim-lspconfig",

    -- EDITOR DISCOVERY
    "folke/which-key.nvim",
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",

    -- EXTRA SYNTAX HIGHLIGHTING
    "lewis6991/gitsigns.nvim",
    "brenoprata10/nvim-highlight-colors",
    "lukas-reineke/indent-blankline.nvim.git",

    -- RICING
    "echasnovski/mini.starter",
    "echasnovski/mini.statusline",

    -- ABILITIES (vimscript)
    "jiangmiao/auto-pairs.git",
    "mg979/vim-visual-multi",
}

local reply = function()
    local dir = vim.fn.stdpath('config') .. '/pack/plugins/start2'
    vim.fn.mkdir(dir, 'p')

    for _, repo in ipairs(download) do
        local name = repo:match(".*/(.*)")
        local download = string.format("git clone https://github.com/%s.git %s/%s", repo, dir, name)
        os.execute(download)
    end
end

return reply()
