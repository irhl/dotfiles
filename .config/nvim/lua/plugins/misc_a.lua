-- nvim-telescope/telescope.nvim
require('telescope').setup {
    pickers = {
    find_files = { hidden = true },
  },
}

vim.o.timeout    = true;
vim.o.timeoutlen = 300;

-- folke/noice.nvim
require("noice").setup({
  presets = {
    bottom_search = false, -- use a classic bottom cmdline for search
    command_palette = true, -- position the cmdline and popupmenu together
    long_message_to_split = true, -- long messages will be sent to a split
    inc_rename = false, -- enables an input dialog for inc-rename.nvim
    lsp_doc_border = false, -- add a border to hover docs and signature help
  },
})

-- folke/which-key.nvim
require 'which-key'.setup {
	icons = {
		group      = '+',
		breadcrumb = '=',
		separator  = '->',
	}
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

-- brenoprata10/nvim-highlight-colors
require("nvim-highlight-colors").setup {
    -- background
    -- foreground
    -- first_column
    render = 'foreground',
    enable_named_colors = false,
    enable_tailwind = false,
}

-- lukas-reineke/indent-blankline.nvim.git
require("indent_blankline").setup {
    show_end_of_line = true,
}

vim.g.indent_blankline_bufname_exclude = {
  'abc.cht',
  '.*.txt'
}

-- kana.vim
vim.g.eskk_directory = {
    path = "~/.config/nvim/skk",
}

vim.g.eskk_dictionary = {
    path = "~/.config/nvim/skk/SKK-JISYO.L",
    sorted = 1,
    encoding = "utf-8",
}

vim.g.eskk_large_dictionary = {
    path = "~/.config/nvim/skk/SKK-JISYO.L",
    sorted = 1,
    encoding = "euc-jp",
}

vim.g.eskk_kakutei_when_unique_candidate = 1
vim.g.eskk_enable_completion = 0
vim.g.eskk_no_default_mappings = 1
vim.g.eskk_keep_state = 0
vim.g.eskk_egg_like_newline = 1
