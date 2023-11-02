vim.o.background = 'dark'

--[[

      +------------------+
      |       ,-_,.      |
      |     ,( _  ))     |       irhl's personal colorscheme
      |     7 (_) 77     |       and is based on flowershop.
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

--]]

local highlight = {
  -- * String *
  Normal      = { fg = '#f0d8bb', bg = '#514036' },
  String      = { fg = '#dbc1a2', bg = 'NONE' },

  -- * Common *
  Constant    = { fg = '#f0d8bb', bg = 'NONE' },
  Function    = { fg = '#eed6b8', bg = 'NONE' },
  Identifier  = { fg = '#eed6b8', bg = 'NONE' },
  Type        = { fg = '#e0c4a3', bg = 'NONE' },
  PreProc     = { fg = '#e0c4a3', bg = 'NONE' },
  Statement   = { fg = '#e2b697', bg = 'NONE' },
  SpecialKey  = { fg = '#b6c693', bg = 'NONE' },
  Special     = { fg = '#b6c693', bg = 'NONE' },
  Repeat      = { fg = '#efcca0', bg = 'NONE' },
  Operator    = { fg = '#f4c1ba', bg = 'NONE' },
  Float       = { fg = '#f2b3a9', bg = 'NONE' },
  Number      = { fg = '#f2b3a9', bg = 'NONE' },
  NonText     = { fg = '#f0d8bb', bg = 'NONE' },
  TODO        = { fg = '#f0d8bb', bg = 'NONE' },
  Title       = { fg = '#f0d8bb', bg = 'NONE' },
  Underlined  = { fg = '#f0d8bb', bg = 'NONE' },

  -- * Utility *
  Comment     = { fg = '#B2957f', bg = 'NONE'    },
  LineNR      = { fg = '#5C483E', bg = '#514036' },
  SignColumn  = { fg = '#514036', bg = '#514036' },
  Search      = { fg = '#514036', bg = '#f0d8bb' },
  IncSearch   = { fg = '#514036', bg = '#f0d8bb' },
  Substitute  = { fg = '#514036', bg = '#f0d8bb' },
  MatchParen  = { fg = '#514036', bg = '#f0d8bb' },
  Visual      = { fg = '#514036', bg = '#f0d8bb' },
  DiffAdd     = { fg = '#C4A99B', bg = 'NONE'    },
  DiffText    = { fg = '#C4A99B', bg = 'NONE'    },
  DiffChange  = { fg = '#b2957f', bg = 'NONE'    },
  DiffDelete  = { fg = '#b2957f', bg = 'NONE'    },
  Directory   = { fg = '#f0d8bb', bg = 'NONE'    },

  -- * Messages *
  Error       = { fg = '#f0d8bb', bg = 'NONE' },
  ErrorMsg    = { fg = '#f0d8bb', bg = 'NONE' },
  WarningMsg  = { fg = '#f0d8bb', bg = 'NONE' },
  MoreMsg     = { fg = '#f0d8bb', bg = 'NONE' },
  Question    = { fg = '#f0d8bb', bg = 'NONE' },

  DiagnosticError = { fg = '#f8d695', bg = 'NONE' },
  DiagnosticWarn  = { fg = '#f8d695', bg = 'NONE' },
  Pmenu           = { fg = '#f8d695', bg = 'NONE' },
  PmenuKind       = { fg = '#f8d695', bg = 'NONE' },
  PmenuExtra      = { fg = '#f8d695', bg = 'NONE' },
  PmenuSel        = { fg = '#f8d695', bg = 'NONE' },

  -- * Toys * --
  WhichKeyFloat   = { fg = '#f0d8bb', bg = '#5C483E' },

  TelescopeNormal          = { fg = '#806b5d', bg = '#47392f' },
  TelescopeBorder          = { fg = '#47392f', bg = '#47392f' },
  TelescopeSelection       = { fg = '#f0d8bb', bg = 'NONE'    },

  TelescopeResultsNormal   = { fg = '#806b5d', bg = '#47392f' },
  TelescopeResultsTitle    = { fg = '#47392f', bg = '#47392f' },
  TelescopeResultsBorder   = { fg = '#47392f', bg = '#47392f' },

  TelescopePromptNormal    = { fg = '#f0d8bb', bg = '#5c483e' },
  TelescopePromptTitle     = { fg = '#5c483e', bg = '#5c483e' },
  TelescopePromptBorder    = { fg = '#5c483e', bg = '#5c483e' },
  TelescopePromptPrefix    = { fg = '#b6c693', bg = '#b6c693' },

  TelescopePreviewNormal   = { fg = '#514036', bg = '#514036' },
  TelescopePreviewTitle    = { fg = '#514036', bg = '#514036' },
  TelescopePreviewBorder   = { fg = '#514036', bg = '#514036' },
  TelescopePreviewMessage  = { fg = '#514036', bg = '#514036' },

  MiniStatuslineModeNormal = { fg = '#47392f', bg = '#f0d8bb' },
  MiniStatuslineFilename   = { fg = '#806b5d', bg = '#47392f' },
  MiniStatuslineDevinfo    = { fg = '#47392f', bg = '#f0d8bb' },
  MiniStatuslineFileinfo   = { fg = '#f0d8bb', bg = '#635044' },

  IndentBlanklineChar               = { link = "LineNr"   },
  IndentBlanklineSpaceChar          = { link = "LineNr"   },
  IndentBlanklineSpaceCharBlankline = { link = "LineNr"   },
  IndentBlanklineContextStart       = { link = "LineNr"   },
  IndentBlanklineContextChar        = { link = "Constant" },
}

local reply = function()
    for hl, col in pairs(highlight) do
        vim.api.nvim_set_hl(0, hl, col)
    end
end

return reply()
