local M = {}

M.highlight = {
  Normal      = { fg = '#888074', bg = '#fbeedb' },
  String      = { fg = '#888074', bg = 'NONE' },

  Constant    = { fg = '#c2ac97', bg = 'NONE' },
  Function    = { fg = '#938c80', bg = 'NONE' },
  Identifier  = { fg = '#888074', bg = 'NONE' },
  Type        = { fg = '#888074', bg = 'NONE' },
  PreProc     = { fg = '#d7cabe', bg = 'NONE' },
  Statement   = { fg = '#b6a699', bg = 'NONE' },
  SpecialKey  = { fg = '#888074', bg = 'NONE' },
  Special     = { fg = '#888074', bg = 'NONE' },
  Repeat      = { fg = '#b6a699', bg = 'NONE' },
  Operator    = { fg = '#acc7b9', bg = 'NONE' },
  Float       = { fg = '#cfb9ca', bg = 'NONE' },
  Number      = { fg = '#cfb9ca', bg = 'NONE' },
  NonText     = { fg = '#888074', bg = 'NONE' },
  TODO        = { fg = '#888074', bg = 'NONE' },
  Title       = { fg = '#888074', bg = 'NONE' },
  Underlined  = { fg = '#888074', bg = 'NONE' },

  Comment     = { fg = '#d7cabe', bg = 'NONE'    },
  LineNR      = { fg = '#efe2d0', bg = '#fbeedb' },
  SignColumn  = { fg = '#fbeedb', bg = '#fbeedb' },
  Search      = { fg = '#fbeedb', bg = '#888074' },
  IncSearch   = { fg = '#fbeedb', bg = '#888074' },
  Substitute  = { fg = '#fbeedb', bg = '#888074' },
  MatchParen  = { fg = '#fbeedb', bg = '#888074' },
  Visual      = { fg = 'NONE', bg = '#f3e3ca' },
  DiffAdd     = { fg = '#888074', bg = 'NONE'    },
  DiffText    = { fg = '#888074', bg = 'NONE'    },
  DiffChange  = { fg = '#888074', bg = 'NONE'    },
  DiffDelete  = { fg = '#888074', bg = 'NONE'    },
  Directory   = { fg = '#888074', bg = 'NONE'    },

  Error       = { fg = '#888074', bg = 'NONE' },
  ErrorMsg    = { fg = '#888074', bg = 'NONE' },
  WarningMsg  = { fg = '#888074', bg = 'NONE' },
  MoreMsg     = { fg = '#888074', bg = 'NONE' },
  Question    = { fg = '#888074', bg = 'NONE' },

  DiagnosticError = { fg = '#888074', bg = 'NONE' },
  DiagnosticWarn  = { fg = '#888074', bg = 'NONE' },
  Pmenu           = { fg = '#888074', bg = 'NONE' },
  PmenuKind       = { fg = '#888074', bg = 'NONE' },
  PmenuExtra      = { fg = '#888074', bg = 'NONE' },
  PmenuSel        = { fg = '#888074', bg = 'NONE' },

  WhichKeyFloat   = { fg = '#888074', bg = '#fbeedb' },

  MiniStatuslineModeNormal = { fg = '#fbeedb', bg = '#fbeedb' },
  MiniStatuslineFilename   = { fg = '#fbeedb', bg = '#fbeedb' },
  MiniStatuslineDevinfo    = { fg = '#fbeedb', bg = '#fbeedb' },
  MiniStatuslineFileinfo   = { fg = '#888074', bg = '#fbeedb' },

  IndentBlanklineChar               = { link = "LineNr"   },
  IndentBlanklineSpaceChar          = { link = "LineNr"   },
  IndentBlanklineSpaceCharBlankline = { link = "LineNr"   },
  IndentBlanklineContextStart       = { link = "LineNr"   },
  IndentBlanklineContextChar        = { link = "Constant" },
}

return M
