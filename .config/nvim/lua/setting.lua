local set = {
  {"scrolloff",     8    },
  {"sidescrolloff", 8    },
  {"showtabline",   0    },
  {"laststatus",    0    },
  {"timeoutlen",    250  },
  {"updatetime",    500  },

  {"lazyredraw",    true },
  {"ttyfast",       true },

  {"backup",        false},
  {"swapfile",      false},
  {"undofile",      true },

  {"wrap",          true },
  {"number",        false},
  {"showmode",      false},
  {"expandtab",     false},
  {"hlsearch",      false},
  {"incsearch",     true },
  {"ignorecase",    true },
  {"smartcase",     true },
  {"smartindent",   true },
  {"splitbelow",    false},
  {"splitright",    true },
  {"termguicolors", true },

  {"mouse",         ""       },
  {"guicursor",     "i:block"},
  {"signcolumn",    "yes:2"  },
  {"fillchars",     "eob: "  },
  {"fileencoding",  "utf-8"  },

  {"clipboard",     "unnamedplus"},
  {"completeopt",  {"menuone", "noselect"} },
  {"undodir",      string.format('%s/undodir', vim.fn.stdpath('cache')) },
}

local reply = function()
  for _, v in pairs(set) do
    vim.opt[v[1]] = v[2]
  end
end

-- disable ruler completely
vim.api.nvim_create_autocmd(
  { "BufRead, BufNewFile" },
  { pattern = { "*" }, command = "set noruler",
})

-- trim trailing whitespaces
vim.api.nvim_create_autocmd({ "BufWritePre" }, {
  pattern = { "*" },
  command = [[%s/\s\+$//e]],
})

-- load xresources on save
vim.api.nvim_create_autocmd({ "BufWritePost" }, {
  pattern = vim.env.HOME .. '/.Xresources',
  command = '!xrdb %',
})

return reply()
