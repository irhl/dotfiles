local M = {}

M.keymaps = {
    silent = {
        -- cursor movement
        {{'v', 'n'}, 'w', "0"},
        {{'v', 'n'}, 'ee', "$"},
        -- cursor travel
        {{'v', 'n'}, 'es', ":mark o <cr>"},
        {{'v', 'n'}, 'er', "'o"},
        -- telescope
        {'n', 'ff', require('telescope.builtin').find_files, {} },
        {'n', 'fg', require('telescope.builtin').live_grep, {} },
        {'n', 'fb', require('telescope.builtin').buffers, {} },
        {'n', 'fv', require('telescope.builtin').help_tags, {} },
    },
    normal = {
        -- search & replace
        {{'v', 'n'}, '<C-f>', ':%s/'},
        -- japanese input eskk
        {'n', '<C-i>', ":lua kana()<cr>"},
        -- buffer manipulation
        {'n', '<C-z>', vim.cmd.bprev},
        {'n', '<C-a>', vim.cmd.bnext},
        -- file manipulation
        {'n', '<C-c>', vim.cmd.exit},
        {'n', '<C-s>', vim.cmd.write},
        {'n', 'ms', ":w !ssu -- tee % >/dev/null <cr>"},
        {'n', 'mc', ":!ssu -- make install <cr>"},
    },
}

M.autocmd = {
    -- trim trailing whitespaces
    {'BufWritePre', '*', '%s/\\s\\+$//e'},
}

M.options = {
    {"showtabline", 0},
    {"laststatus", 0},
    {"scrolloff", 8},
    {"sidescrolloff", 8},
    {"timeoutlen", 250},
    {"updatetime", 500},
    {"lazyredraw", true},
    {"ttyfast", true},
    {"backup", false},
    {"swapfile", false},
    {"undofile", true},
    {"wrap", false},
    {"ruler", false},
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
    {"undodir", string.format('%s/undodir', vim.fn.stdpath('cache'))},
}

M.plugins = {
    -- disable built-in plugins (38/39)
    builtins = {
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
    },
    after = {
        dir = vim.fn.expand('$HOME') .. '/.config/nvim/lua/plugins/',
        rdir = vim.fn.readdir(vim.fn.expand('$HOME') .. '/.config/nvim/lua/plugins/'),
    },
}

return M
