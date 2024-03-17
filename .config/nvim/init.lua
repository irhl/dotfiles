-- disable builtin plugins and providers
local disable_builtin_plugins = {"2html_plugin", "getscript", "getscriptPlugin", "gzip", "logipat", "netrw", "netrwPlugin", "netrwSettings", "netrwFileHandlers", "matchit", "tar", "tarPlugin", "rrhelper", "spellfile_plugin", "vimball", "vimballPlugin", "zip", "zipPlugin", "tutor", "rplugin", "syntax", "synmenu", "optwin", "compiler", "bugreport", "ftplugin"}
local disable_builtin_providers = {"node", "perl", "ruby"}
for _, plugin in pairs(disable_builtin_plugins) do
  vim.g[("loaded_" .. plugin)] = 1
end
for _, provider in ipairs(disable_builtin_providers) do
  vim.g[("loaded_" .. provider .. "_provider")] = 0
end

-- :help lua-loader
vim.loader.enable()

-- load plugins (they are stored at ~/.config/nvim/pack/start/plugins)
-- to download them, do :lua gitPull
vim.cmd('packloadall')
vim.o.background = 'light'

local source = vim.fn.expand(os.getenv('HOME') .. '/.config/nvim/lua/')
local read = vim.fn.readdir(source)
for _, plugin in ipairs(read) do
    if plugin:match('%.lua$') then
        pcall(dofile, source .. plugin)
    end
end

-- in case if errors persist and some files are unable to load
vim.opt.clipboard:append('unnamedplus')
vim.opt.undodir = vim.fn.expand('~/.cache/nvim/undodir')
vim.opt.undofile = true
vim.o.swapfile = false

-- no idea what this does, but i'd say it is beneficial to have it
vim.g.vimwiki_global_ext = 0
