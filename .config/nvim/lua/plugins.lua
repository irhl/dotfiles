-- :help lua-loader
vim.loader.enable()

-- load plugins
vim.cmd('packloadall')

-- load plugin config
local dir = '/home/irhl/.config/nvim/lua/plugins/'
local read = vim.fn.readdir(dir)

-- disable built-in plugins (34)
local builtins = {
  "2html_plugin", "archlinux",
  "bugreport", "compiler",
  "ftplugin", "fzf",
  "sleuth", "syntax", "synmenu",
  "spellfile",  "spellfile_plugin",
  "optwin", "matchit", -- "matchparen",
  "rrhelper", "rplugin", "logipat",
  "gtags", "getscript", "getscriptPlugin",
  "man", "tutor", "tutor_mode_plugin",
  "tar", "tarPlugin", "zip", "zipPlugin",
  "gzip", "vimball", "vimballPlugin",
  "vimgrep", "netrw", "netrwPlugin",
  "netrwSettings", "netrwFileHandlers",
  "node_provider", "ruby_provider",
  "python3_provider", "perl_provider",
}

local reply = function()
  for _, plugin in ipairs(read) do
    if plugin:match('%.lua$') then
        pcall(dofile, dir .. plugin)
    end
  end

  for _, plugin in ipairs(builtins) do
    vim.g["loaded_" .. plugin] = 1
  end
end

return reply()
