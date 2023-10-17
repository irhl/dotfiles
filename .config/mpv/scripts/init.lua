local mp = require 'mp'
local home = os.getenv("HOME")

local scripts = {
    -- https://github.com/stax76/mpv-scripts
    "/.config/mpv/scripts/common/history.lua"
}

local reply = function()
  for _, script in pairs(scripts) do
    mp.commandv("load-script", home .. script)
  end
end

return reply()
