local script_directory = debug.getinfo(1, 'S').source:sub(2):match('(.*/)')
package.path = script_directory .. '?.lua;' .. package.path

local mp = require 'mp'
local config = require('config')

for key, action in pairs(config.keymaps) do
    local config = {}
    if key == 'left' or key == 'right'
	    or key == 'z' or key == 'Z'
	    or key == 'x' or key == 'X'
	    then
        config = { 'repeatable' }
    end

    mp.add_forced_key_binding(key, function()
        mp.command(action)
    end, unpack(config))
end

for opt, arg in pairs(config.options) do
    mp.set_property('options/' .. opt:gsub('_', '-'), arg)
end
