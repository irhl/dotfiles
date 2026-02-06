local dir_script = debug.getinfo(1, 'S').source:match("@(.*[/\\])")
package.path = dir_script .. '?.lua;' .. package.path

local mp = require 'mp'

local fc = require 'fc'
local ac = require 'ac'

mp.set_property("msg-level", "all=no")

local function action_wrap(action)
    return function()
       if type(action) == "function" then
            action()
       else
            mp.command(action)
        end
    end
end

for pattern, action in pairs(ac.keymaps) do
   local callback = action_wrap(action)
       for key in pattern:gmatch("[^,]+") do
        mp.add_forced_key_binding(key, callback, "repeatable")
    end
end

for option_name, value in pairs(ac.options) do
    local option_fmt = option_name:gsub("_", "-")
    mp.set_property("options/" .. option_fmt, value)
end
