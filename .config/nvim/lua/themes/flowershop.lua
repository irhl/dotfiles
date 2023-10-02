-- Name:       flowershop (monochrome)
-- Version:    x.x
-- Maintainer: github.com/irhl
-- License:    The MIT License (MIT)

--          +------------------+
--          |       ,-__.      |
--          |     ,( _  ))     |
--          |     7 (_) 77     |
--          |     ((   :))     |
--          |      ~__>~'      |
--          |       cY?'       |
--          |       `l,__      |
--          |        l7-'      |
--          |       ;l         |
--          |       _i_,       |
--          |      l___l       |
--          |      \___/ ~irhl |
--          |------------------|
--          +------------------+

-- apply light themes properly
vim.o.background = 'light'

non = 'NONE'
fg1 = '#47392f'
fg2 = '#514036'

bg4 = '#887468'
bg3 = '#b5a192'
bg2 = '#ebdccd'
bg1 = '#fbeedb'

co1 = '#c4a99b'
co2 = '#c4a99b'

local highlight = {
  -- * Common *
  Normal      = { fg = fg1, bg = non },
  String      = { fg = fg1, bg = non },

  -- * Uncommon *
  Type        = { fg = fg1, bg = non },
  Float       = { fg = fg1, bg = non },
  PreProc     = { fg = fg1, bg = non },
  Constant    = { fg = fg1, bg = non },

  -- * Function *
  Function    = { fg = fg1, bg = non },
  Identifier  = { fg = fg1, bg = non },
  Statement   = { fg = fg1, bg = non },
  SpecialKey  = { fg = fg1, bg = non },
  Special     = { fg = bg4, bg = non },
  Repeat      = { fg = fg1, bg = non },
  Operator    = { fg = co2, bg = non },
  Number      = { fg = co1, bg = non },
  NonText     = { fg = fg1, bg = non },
  TODO        = { fg = fg1, bg = non },
  Title       = { fg = fg1, bg = non },
  Underlined  = { fg = fg1, bg = non },

  -- * Utility *
  Comment     = { fg = bg4, bg = bg1 },
  LineNR      = { fg = bg4, bg = bg1 },
  SignColumn  = { fg = fg1, bg = non },

  Search      = { fg = bg2, bg = fg1 },
  IncSearch   = { fg = bg2, bg = fg1 },
  Substitute  = { fg = bg2, bg = fg1 },
  MatchParen  = { fg = bg2, bg = fg1 },
  Visual      = { fg = bg2, bg = fg1 },

  Directory   = { fg = fg1, bg = non },
  DiffAdd     = { fg = fg1, bg = non },
  DiffChange  = { fg = fg1, bg = non },
  DiffDelete  = { fg = fg1, bg = non },
  DiffText    = { fg = fg1, bg = non },

  DiagnosticError = { fg = fg1, bg = non },
  DiagnosticWarn  = { fg = fg1, bg = non },

  -- * Messages *
  Error       = { fg = fg1, bg = non },
  ErrorMsg    = { fg = fg1, bg = non },
  WarningMsg  = { fg = fg1, bg = non },
  MoreMsg     = { fg = fg1, bg = non },
  Question    = { fg = fg1, bg = non },

  -- * Menu * --
  Pmenu           = { fg = fg2, bg = non },
  PmenuKind       = { fg = fg2, bg = non },
  PmenuExtra      = { fg = fg2, bg = non },
  PmenuSel        = { fg = fg2, bg = non },

  WhichKeyFloat   = { fg = fg1, bg = bg1 },

  TelescopeNormal           = { fg = bg5, bg = bg1 },
  TelescopeBorder           = { fg = bg1, bg = bg1 },
  TelescopeSelection        = { fg = fg1, bg = non },

  TelescopeResultsNormal    = { fg = bg3, bg = bg2 },
  TelescopeResultsTitle     = { fg = bg2, bg = bg2 },
  TelescopeResultsBorder    = { fg = bg2, bg = bg2 },

  TelescopePromptNormal     = { fg = fg1, bg = bg4 },
  TelescopePromptTitle      = { fg = bg4, bg = bg4 },
  TelescopePromptBorder     = { fg = bg4, bg = bg4 },
  TelescopePromptPrefix     = { fg = fg1, bg = fg1 },

  TelescopePreviewNormal    = { fg = bg1, bg = bg1 },
  TelescopePreviewTitle     = { fg = bg1, bg = bg1 },
  TelescopePreviewBorder    = { fg = bg1, bg = bg1 },
  TelescopePreviewMessage   = { fg = bg1, bg = bg1 },

  MiniStatuslineFilename    = { fg = bg1, bg = bg1 },
  MiniStatuslineFileinfo    = { fg = fg1, bg = bg2 },
  MiniStatuslineDevinfo     = { fg = bg1, bg = fg2 },

  MiniStatuslineModeNormal  = { fg = bg1, bg = bg1 },
  MiniStatuslineModeInsert  = { fg = bg1, bg = bg1 },
  MiniStatuslineModeVisual  = { fg = bg1, bg = bg1 },
  MiniStatuslineModeReplace = { fg = bg1, bg = bg1 },
  MiniStatuslineModeCommand = { fg = bg1, bg = bg1 },

  -- * Links *
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
