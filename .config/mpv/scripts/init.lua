local mp = require 'mp'
local home = os.getenv("HOME")

local scripts = {
    -- mpv-player/mpv/tree/master/TOOLS/lua
    "/.config/mpv/scripts/common/autoload.lua"
}

local reply = function()
  for _, script in pairs(scripts) do
    mp.commandv("load-script", home .. script)
  end
end

return reply()
