-- nvim-treesitter/nvim-treesitter
require 'nvim-treesitter.configs'.setup {
    ensure_installed = {
      'c',
      'lua'
    },

    ignore_install = {
      'cpp',
      'javascript',
      'typescript',
      'npm',
      'python',
      'vim',
      'vimdoc'
    },

    sync_install = false,
    auto_install = false,

  highlight = {
    enable = true,
  },
}
