-- if telescope plugin load fails, return an empty table
-- this should still load the file even if plugin errors occur
local telescope
local success, plugin = pcall(require, 'telescope.builtin')

telescope = success and plugin or setmetatable({}, {
    __index = function()
        return function() end
    end
})

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
    {'n', 'ff', telescope.find_files, {} },
    {'n', 'fg', telescope.live_grep,  {} },
    {'n', 'fb', telescope.buffers,    {} },
    {'n', 'fv', telescope.help_tags,  {} },
  },
  standard = {
    -- quick search & replace
    {{'v', 'n'}, '<C-f>', ':%s/'},

    -- japanese input eskk
    {'n', '<C-i>', ":lua kana()<cr>"},

    -- buffer manipulation
    {'n', '<C-z>', vim.cmd.bprev},
    {'n', '<C-a>', vim.cmd.bnext},

    -- file manipulation
    {'n', '<C-c>', vim.cmd.exit},
    {'n', '<C-s>', vim.cmd.write},
  },
}

M.options = {
    wrap          = false,
    expandtab     = false,
    hlsearch      = false,
    incsearch     = true,
    ignorecase    = true,
    smartcase     = true,
    smartindent   = true,

    number        = false,
    ruler         = false,
    showcmd       = false,
    showmode      = false,
    showtabline   = 0,
    signcolumn    = "yes:2",
    fileencoding  = "utf-8",
    clipboard     = "unnamedplus",
    completeopt   = {"menuone", "noselect"},
    fillchars     = {eob = " ", fold = " ", vert = " "},

    -- cursor view
    mouse         = "",
    guicursor     = "i:block",
    cursorline    = true,
    scrolloff     = 8,
    sidescrolloff = 8,

    -- performance
    lazyredraw    = true,
    ttyfast       = true,
    termguicolors = true,

    -- file manipulation
    backup        = false,
    swapfile      = false,
    undofile      = true,
    undodir       = vim.fn.expand("~/.cache/nvim/undodir"),
}

M.theme = {
    "Function",        "#928d87",   "NONE",
    "Repeat",          "#928d87",   "NONE",
    "Conditional",     "#928d87",   "NONE",
    "Statement",       "#928d87",   "NONE",

    "Normal",          "#44403f",   "#fcf2e8",
    "String",          "#44403f",   "NONE",

    "Constant",        "#44403f",   "NONE",
    "Identifier",      "#44403f",   "NONE",
    "Type",            "#44403f",   "NONE",
    "PreProc",         "#44403f",   "NONE",
    "SpecialKey",      "#44403f",   "NONE",
    "Special",         "#44403f",   "NONE",
    "Operator",        "#44403f",   "NONE",
    "Float",           "#6b6564",   "NONE",
    "Number",          "#6b6564",   "NONE",
    "NonText",         "#44403f",   "NONE",
    "TODO",            "#44403f",   "NONE",
    "Title",           "#44403f",   "NONE",
    "Underlined",      "#44403f",   "NONE",

    "Comment",         "#cdc4bc",   "NONE",
    "LineNR",          "#ece2d9",   "NONE",
    "SignColumn",      "#fcf2e8",   "NONE",
    "Search",          "#fcf2e8",   "#44403f",
    "IncSearch",       "#fcf2e8",   "#44403f",
    "Substitute",      "#fcf2e8",   "#44403f",
    "MatchParen",      "#fcf2e8",   "#44403f",
    "Visual",          "NONE",      "#f3e9df",
    "DiffAdd",         "#44403f",   "NONE",
    "DiffText",        "#44403f",   "NONE",
    "DiffChange",      "#44403f",   "NONE",
    "DiffDelete",      "#44403f",   "NONE",
    "Directory",       "#44403f",   "NONE",
    "Folded",          "NONE",      "#e8ded5",

    "Error",           "#44403f",   "NONE",
    "ErrorMsg",        "#44403f",   "NONE",
    "WarningMsg",      "#44403f",   "NONE",
    "MoreMsg",         "#44403f",   "NONE",
    "Question",        "#44403f",   "NONE",
    "DiagnosticError", "#44403f",   "NONE",
    "DiagnosticWarn",  "#44403f",   "NONE",
    "DiagnosticHint",  "#44403f",   "NONE",

    "CmpItemKind",     "#44403f",   "NONE",
    "Pmenu",           "#44403f",   "NONE",
    "PmenuKind",       "#44403f",   "NONE",
    "PmenuExtra",      "#44403f",   "NONE",
    "PmenuSel",        "#44403f",   "NONE",
    "PmenuThumb",      "#44403f",   "NONE",
    "Statusline",      "#44403f",   "NONE",
    "StatuslineNC",    "#44403f",   "NONE",
    "TabLine",         "#44403f",   "NONE",
    "CursorLine",      "NONE",      "#f3e9df",
    "CursorLineNR",    "#44403f",   "NONE",

    "@ibl.indent.char.1",     "#ece2d9", "NONE", 
    "@ibl.whitespace.char.1", "#ece2d9", "NONE", 
}

return M
