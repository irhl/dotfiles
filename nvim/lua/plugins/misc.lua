require('telescope').setup {
    pickers = {
    find_files = { hidden = true },
  },
}

vim.o.timeout    = true;
vim.o.timeoutlen = 300;

require 'which-key'.setup {
	icons = {
		group      = '+',
		breadcrumb = '=',
		separator  = '->',
	}
}

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

require("nvim-highlight-colors").setup {
    -- background
    -- foreground
    -- first_column
    render = 'background',
    enable_named_colors = false,
    enable_tailwind = false,
}

vim.g.indent_blankline_bufname_exclude = {
  'abc.cht',
  '.*.txt'
}

require("indent_blankline").setup {
    show_end_of_line = true,
}
