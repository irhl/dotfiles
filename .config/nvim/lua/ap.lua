-- nvim-telescope/telescope.nvim
-- + nvim-lua/plenary.nvim
require('telescope').setup{
  pickers = {
    find_files = {
      hidden = true,
      theme = "dropdown",
    }
  },
}

vim.o.timeout    = true;
vim.o.timeoutlen = 300;

-- brenoprata10/nvim-highlight-colors
require("nvim-highlight-colors").setup {
    render = 'foreground',
    enable_named_colors = false,
    enable_tailwind = false,
}

-- lewis6991/gitsigns.nvim
require('gitsigns').setup {
    signs = {
      add          = { text = '++' },
      change       = { text = '::' },
      delete       = { text = '--' },
      changedelete = { text = '~~' },
      topdelete    = { text = 'xx' },
      untracked    = { text = '..' },
    },
      signcolumn = true,
      numhl      = false,
      linehl     = false,
      word_diff  = false,

    watch_gitdir = {
      follow_files = false
    },
}

-- lukas-reineke/indent-blankline.nvim.git
require("ibl").setup {
  scope = { enabled = false },
}

require "ibl".overwrite {
  exclude = { filetypes = {"text", "markdown"} }
}

-- kana.vim, written by Rory McCann
local basepath = os.getenv('HOME') .. "/.config/nvim/pack/plugins/start/skk/"

vim.g.eskk_directory = {
    path = basepath,
}

vim.g.eskk_dictionary = {
    path = basepath .. "SKK-JISYO.L",
    sorted = 1,
    encoding = "utf-8",
}

vim.g.eskk_large_dictionary = {
    path = basepath .. "SKK-JISYO.L",
    sorted = 1,
    encoding = "euc-jp",
}

vim.g.eskk_kakutei_when_unique_candidate = 1
vim.g.eskk_enable_completion = 0
vim.g.eskk_no_default_mappings = 1
vim.g.eskk_keep_state = 0
vim.g.eskk_egg_like_newline = 1
