local repositories = {
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

local destination = vim.fn.stdpath('config')
.. '/pack/plugins/start'
vim.fn.mkdir(destination, 'p')

local reply = function()
  for _, repo in ipairs(repositories) do
      local repo_name = repo:match(".*/(.*)")
      local clone_command = string.format(
      "git clone https://github.com/%s.git %s/%s",
      repo, destination, repo_name)
      os.execute(clone_command)
  end
end
