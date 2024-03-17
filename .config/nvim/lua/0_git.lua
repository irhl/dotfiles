local receive_full = {
     -- EDITOR ENLIGHTENMENT
    'nvim-treesitter/nvim-treesitter',
    'nvim-telescope/telescope.nvim',
    'nvim-lua/plenary.nvim',
    'echasnovski/mini.starter',

    -- EDITOR ENCHANCEMENT
    'mg979/vim-visual-multi',

    -- SYNTAX HIGHLIGHTING
    'lewis6991/gitsigns.nvim',
    'brenoprata10/nvim-highlight-colors',
    'lukas-reineke/indent-blankline.nvim'
}

local receive_raw = {
    -- EDITOR MULTILINGUAL
    'neovim/neovim/master/runtime/keymap/kana.vim',
    'skk-dev/dict/master/SKK-JISYO.L'
}

_G.gitPull = function()
    local url_full = 'https://github.com/'
    local url_raw = 'https://raw.githubusercontent.com/'

    local dir = vim.fn.stdpath('config') .. '/pack/plugins/start'
    local skk = dir .. '/skk'

    vim.fn.mkdir(dir,  'p')
    vim.fn.mkdir(skk, 'p')

    local reply = {}
    for _, repo in ipairs(receive_full) do
        local a = repo:match('.*/(.*)')
        local b = dir .. '/' .. a

        if vim.fn.isdirectory(b) == 0 then
            table.insert(reply, 'git clone ' .. url_full .. repo .. '.git ' .. b)
        end
    end
    for _, repo in ipairs(receive_raw) do
        local a = repo:match('.*/(.*)')
        local b = skk .. '/' .. a

        if vim.fn.filereadable(b) == 0 then
            table.insert(reply, 'curl -o ' .. b .. ' ' .. url_raw .. repo)
        end
    end

    os.execute(table.concat(reply, ' && '))

    io.write("\27[31m", "download complete!", "\27[0m")
    io.flush()
end
