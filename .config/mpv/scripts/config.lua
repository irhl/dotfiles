local script_directory = debug.getinfo(1, "S").source:sub(2):match("(.*/)")
package.path = script_directory .. '?.lua;' .. package.path

local mp = require 'mp'
local ranch = require('ranch')

for opt, arg in pairs(ranch.options) do
    mp.set_property('options/' .. opt:gsub('_', '-'), arg)
end

for key, action in pairs(ranch.keymaps) do
    local ranch = {}
    if key == "left" or key == "right" then
        ranch = { "repeatable" }
    end

    mp.add_forced_key_binding(key, function()
        mp.command(action)
    end, unpack(ranch))
end
