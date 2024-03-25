-- disable builtin plugins and providers
local disable_builtin_plugins = {"2html_plugin", "getscript", "getscriptPlugin", "gzip", "logipat", "netrw", "netrwPlugin", "netrwSettings", "netrwFileHandlers", "matchit", "tar", "tarPlugin", "rrhelper", "spellfile_plugin", "vimball", "vimballPlugin", "zip", "zipPlugin", "tutor", "rplugin", "syntax", "synmenu", "optwin", "compiler", "bugreport", "ftplugin"}
local disable_builtin_providers = {"node", "perl", "ruby"}
for _, plugin in pairs(disable_builtin_plugins) do
  vim.g[("loaded_" .. plugin)] = 1
end
for _, provider in ipairs(disable_builtin_providers) do
  vim.g[("loaded_" .. provider .. "_provider")] = 0
end

--[[@:

to get started, download the plugins with the command below
:lua gitPull()

+-----------------------+
|                       |
|   STARTUP             |
|   -------             |
|   init.lua            |
|                       |
|   USER CONFIG         |
|   ----------          |
|   config.lua          |
|   hachos.lua          |
|                       |
|   USER AFTERPLUGINS   |
|   -----------------   |
|   ap.lua              |
|   ap_starter.lua      |
|   ap_treesitter.lua   |
|                       |
+----------------------]]

-- :help lua-loader
vim.loader.enable()

-- load plugins (they are stored at ~/.config/nvim/pack/start/plugins)
vim.cmd('packloadall')
vim.o.background = 'light'

local source = vim.fn.expand(os.getenv('HOME') .. '/.config/nvim/lua/')
local read = vim.fn.readdir(source)
for _, plugin in ipairs(read) do
    if plugin:match('%.lua$') then
        pcall(dofile, source .. plugin)
    end
end

-- display statusline
vim.cmd [[
  hi left   guifg=#716b67 guibg=#c6dbd8 gui=bold
  hi center guifg=#c6dbd8 guibg=#c6dbd8 gui=NONE
  hi right  guifg=#716b67 guibg=#c6dbd8 gui=bold
]]
  
local function statusline()
  vim.o.statusline = '%#left# %f %m' ..
                     '%#center#  %=' ..
                     '%#right# Ln %l, Col %c '
end
  
statusline()
