require 'plugins.mini';
require 'plugins.misc';
require 'plugins.treesitter';

-- load plugins
vim.cmd('packloadall')

-- :help lua-loader
vim.loader.enable()

-- disable nvim intro
vim.opt.shortmess:append "sI"

-- disable remote providers
vim.g.loaded_python3_provider = 0
vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0

-- disable built-in plugins (34)
local builtins = {
  "2html_plugin",
  "getscript",
  "getscriptPlugin",
  "gzip",
  "logipat",
  "netrw",
  "netrwPlugin",
  "netrwSettings",
  "netrwFileHandlers",
  "matchit",
  -- "matchparen",
  "tar",
  "tarPlugin",
  "rrhelper",
  "spellfile_plugin",
  "vimball",
  "vimballPlugin",
  "zip",
  "zipPlugin",
  "logipat",
  "tutor",
  "rplugin",
  "syntax",
  "synmenu",
  "optwin",
  "compiler",
  "bugreport",
  "ftplugin",
  "archlinux",
  "fzf",
  "tutor_mode_plugin",
  "sleuth",
  "vimgrep"
}

local reply = function()
  for _, plugin in ipairs(builtins) do
    vim.g["loaded_" .. plugin] = 1
  end
end

return reply()
