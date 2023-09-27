local builtin = require('telescope.builtin')
local opt = { noremap = true }

local key = {
  -- cursor movement
  {{'v', 'n'}, 'w',  "0"},
  {{'v', 'n'}, 'ee', "$"},

  -- cursor travel
  {{'v', 'n'}, 'es', ":mark o <cr>"},
  {{'v', 'n'}, 'er', "'o"},

  -- search & replace
  {{'v', 'n'}, '<C-f>', ':%s/'},

  -- window splitting
  {'n', '<C-i>', '<C-w>v'},
  {'n', '<C-i>', '<C-w>s'},

  -- window resize
  {'n', '<C-h>', ':resize -2 <cr>'},
  {'n', '<C-j>', ':resize +2 <cr>'},
  {'n', '<C-k>', ':vertical resize -2 <cr>'},
  {'n', '<C-l>', ':vertical resize +2 <cr>'},

  -- telescope
  {'n', 'ff', builtin.find_files, {} },
  {'n', 'fg', builtin.live_grep,  {} },
  {'n', 'fb', builtin.buffers,    {} },
  {'n', 'fv', builtin.help_tags,  {} },

  -- buffer options
  {'n', '<C-z>', vim.cmd.bprev},
  {'n', '<C-a>', vim.cmd.bnext},

  -- file manipulation
  {'n', '<C-c>', vim.cmd.exit},
  {'n', '<C-s>', vim.cmd.write},

  {'n', 'ms', ":w !ssu -- tee % >/dev/null <cr>"},

  {'n', 'mm', function()
    vim.cmd.luafile('%') end },
}

local reply = function()
  for _, v in pairs(key) do
    vim.keymap.set(v[1], v[2], v[3], opt)
  end
end

return reply()
