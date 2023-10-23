local builtin = require('telescope.builtin')
local keymap_opts = { noremap = true }

local config = {
    key = {
        -- cursor movement
        {{'v', 'n'}, 'w', "0"},
        {{'v', 'n'}, 'ee', "$"},

        -- cursor travel
        {{'v', 'n'}, 'es', ":mark o <cr>"},
        {{'v', 'n'}, 'er', "'o"},

        -- search & replace
        {{'v', 'n'}, '<C-f>', ':%s/'},

        -- indentation
        {'v', '<', '<gv'},
        {'v', '<', '>gv'},

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
        {'n', 'fg', builtin.live_grep, {} },
        {'n', 'fb', builtin.buffers, {} },
        {'n', 'fv', builtin.help_tags, {} },

        -- buffer options
        {'n', '<C-z>', vim.cmd.bprev},
        {'n', '<C-a>', vim.cmd.bnext},

        -- file manipulation
        {'n', '<C-c>', vim.cmd.exit},
        {'n', '<C-s>', vim.cmd.write},
        {'n', 'ms', ":w !ssu -- tee % >/dev/null <cr>"},
        {'n', 'mm', function() vim.cmd.luafile('%') end },
    },
    aucmd = {
        -- disable ruler completely
        {'BufRead', 'BufNewFile',
	'*', 'set noruler'},

        -- trim trailing whitespaces
        {'BufWritePre',
	'*', '%s/\\s\\+$//e'},

	-- write to nsxiv on save
        {'BufWritePost',
	vim.env.HOME .. '/.nsxivrc', '!xrdb %'},
    },
    opt = {
        {"scrolloff", 8},
        {"sidescrolloff", 8},
        {"showtabline", 0},
        {"laststatus", 0},
        {"timeoutlen", 250},
        {"updatetime", 500},
        {"lazyredraw", true},
        {"ttyfast", true},

        {"backup", false},
        {"swapfile", false},
        {"undofile", true},

        {"wrap", false},
        {"number", false},
        {"showmode", false},
        {"expandtab", false},
        {"hlsearch", false},
        {"incsearch", true},
        {"ignorecase", true},
        {"smartcase", true},
        {"smartindent", true},
        {"splitbelow", false},
        {"splitright", true},
        {"termguicolors", true},

        {"mouse", ""},
        {"guicursor", "i:block"},
        {"signcolumn", "yes:2"},
        {"fillchars", "eob: "},
        {"fileencoding", "utf-8"},

        {"clipboard", "unnamedplus"},
        {"completeopt", { "menuone", "noselect" }},
        {"undodir", string.format('%s/undodir',
	vim.fn.stdpath('cache'))},
    }
}

local reply = function()
    for section, command in pairs(config) do
        for _, v in ipairs(command) do
            if section == 'key' then
                vim.keymap.set(
                v[1], v[2], v[3],
                keymap_opts)

            elseif section == 'aucmd' then
                vim.cmd(
                string.format(
                'autocmd %s %s %s',
                v[1], v[2], v[3]))

            elseif section == 'opt' then
                vim.opt[v[1]] = v[2]
            end
        end
    end
end

return reply()
