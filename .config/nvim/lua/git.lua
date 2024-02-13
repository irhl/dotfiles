local receive_full = {
   -- EDITOR ENLIGHTENMENT
  "nvim-treesitter/nvim-treesitter",
  "nvim-telescope/telescope.nvim",
  "nvim-lua/plenary.nvim",
  "neovim/nvim-lspconfig",

  -- EDITOR ENCHANCEMENT
  "jiangmiao/auto-pairs",
  "mg979/vim-visual-multi",

  -- SYNTAX HIGHLIGHTING
  "lewis6991/gitsigns.nvim",
  "brenoprata10/nvim-highlight-colors",
  "lukas-reineke/indent-blankline.nvim",

  -- AESTHETICS
  "echasnovski/mini.starter",
  "echasnovski/mini.statusline",
  "folke/which-key.nvim",
  "folke/noice.nvim",
  "MunifTanjim/nui.nvim",
}

local receive_raw = {
  -- MULTILINGUAL INPUT
  "neovim/neovim/master/runtime/keymap/kana.vim"
}

local call = function()
    local url_github = "https://github.com/"
    local url_github_raw = "https://raw.githubusercontent.com/"

    local dir = vim.fn.stdpath('config') .. '/pack/plugins/start'
    vim.fn.mkdir(dir, 'p')

    local reply = {}
    for _, repo in ipairs(receive_full) do
        local name = repo:match(".*/(.*)")
        table.insert(reply, "git clone " .. url_github .. repo .. ".git " .. dir .. "/" .. name)
    end
    for _, repo in ipairs(receive_raw) do
        local name = repo:match(".*/(.*)")
        table.insert(reply, "curl -o " .. dir .. "/" .. name .. " " .. url_github_raw .. repo)
    end

    local meow = table.concat(reply, " && ")
    os.execute(meow)
end

return call()
