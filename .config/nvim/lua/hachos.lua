local config = require('config')

for _, type in pairs{config.keymaps.silent, config.keymaps.standard} do
    for _, v in ipairs(type) do
        local arg = type == config.keymaps.silent and {noremap = true, silent = true} or nil
        vim.keymap.set(v[1], v[2], v[3], arg)
    end
end

for v1, v2 in pairs(config.options) do
    vim.opt[v1] = v2
end

for i = 1, #config.theme, 3 do
    local hl, fg, bg = config.theme[i], config.theme[i + 1], config.theme[i + 2]
    vim.api.nvim_set_hl(0, hl, { fg = fg, bg = bg })
end

-- display statusline
vim.cmd [[
  hi left  guifg=#716b67 guibg=#f5e1de gui=bold
  hi right guifg=#716b67 guibg=#f5e1de gui=bold
]]

local function statusline()
  vim.o.statusline = '%#left# %f %m %=' ..
                     '%#right# Ln %l, Col %c '
end

statusline()
