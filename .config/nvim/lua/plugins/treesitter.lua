-- nvim-treesitter/nvim-treesitter
require 'nvim-treesitter.configs'.setup {
    ensure_installed = {
      'c',
      'cpp',
      'lua',
      'vim',
      'vimdoc'
    },

    ignore_install = {
      'javascript',
      'typescript',
      'npm',
      'python'
    },

    sync_install = false,
    auto_install = false,

	highlight = {
		enable = true,

		disable = function(_, buf)
			local max_file_size = 100 * 1024; -- 100 KB
			local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf));

			if ok and stats and stats.size > max_file_size then
				return true;
			end
		end
	},
}
