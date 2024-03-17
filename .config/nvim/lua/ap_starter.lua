-- echasnovski/mini.starter
local H = {}

H.default_header = function()
  local hour = tonumber(vim.fn.strftime('%H'))
  local part_id = math.floor((hour + 4) / 8) + 1
  local day_part = ({ 'evening', 'morning', 'afternoon', 'evening' })[part_id]
  local username = vim.loop.os_get_passwd()['username'] or 'USERNAME'

  return ('good %s, %s!'):format(day_part, username)
end

H.default_footer = [[
   ,-_,.
 ,( _  ))
 7 (_) 77
 ((   :))
  ~__>~'
   cY?'
   `l,__
    l7-'
   ;l
   _i_,
  l___l
  \___/
        irhl
]]

local starter = require('mini.starter')
starter.setup({
  header = H.default_header,
  footer = H.default_footer,

  evaluate_single = true,

items = {
    starter.sections.recent_files(5, false, false),

     { section = "Bookmarks",
       name    = "XBPS Packages",
       action  = ":!bb firefox voidlinux.org/packages"},

     { section = "Bookmarks",
       name    = "Steam Client",
       action  = "!bb steam"},
  },
  content_hooks = {
    starter.gen_hook.adding_bullet(),
    starter.gen_hook.aligning('center', 'center'),
    -- starter.gen_hook.indexing('all', { 'Builtin actions' }),
    -- starter.gen_hook.padding(3, 2),
  },
})
