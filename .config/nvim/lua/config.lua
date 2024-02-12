-- :help lua-loader
vim.loader.enable()

-- load plugins
vim.cmd('packloadall')

vim.o.background = 'light'
local theme = require('theme')

local ranch = require('ranch')
local builtin = require('telescope.builtin')

kana = function()
    vim.cmd('set keymap=' .. (vim.g.kana and '' or 'kana'))
    vim.g.kana = not vim.g.kana
end

for _, section in pairs{ranch.keymaps.silent, ranch.keymaps.normal} do
    for _, v in ipairs(section) do
        local arg = section == ranch.keymaps.silent
		and {noremap = true, silent = true} or nil
        vim.keymap.set(v[1], v[2], v[3], arg)
    end
end

for _, v in ipairs(ranch.autocmd) do
    vim.cmd(string.format('autocmd %s %s %s', v[1], v[2], v[3]))
end

for _, v in ipairs(ranch.options) do
    vim.opt[v[1]] = v[2]
end

for _, plugin in ipairs(ranch.plugins.after.rdir) do
    if plugin:match('%.lua$') then
        pcall(dofile, ranch.plugins.after.dir .. plugin)
    end
end

for _, plugin in ipairs(ranch.plugins.builtins) do
    vim.g["loaded_" .. plugin] = 1
end

for hl, col in pairs(theme.highlight) do
    vim.api.nvim_set_hl(0, hl, col)
end

--[[
      +------------------+
      |       ,-_,.      |
      |     ,( _  ))     |
      |     7 (_) 77     |
      |     ((   :))     |
      |      ~__>~'      |
      |       cY?'       |
      |       `l,__      |
      |        l7-'      |
      |       ;l         |
      |       _i_,       |
      |      l___l       |
      |      \___/ ~irhl |
      |------------------|
      +------------------+

      An artwork I created, dated somewhere in 2023.
      It's a portrait for my old flowershop colorscheme.
--]]
