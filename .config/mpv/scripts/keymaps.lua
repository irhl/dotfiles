local mp = require 'mp'

local key = {
    ["q"] = "quit",
    ["m"] = "cycle mute",
    ["0"] = "add volume 15",
    ["9"] = "add volume -15",
    ["a"] = "playlist-next",
    ["z"] = "playlist-prev",
    ["left"]  = "seek -10",
    ["right"] = "seek 10",
    ["space"] = "cycle pause",

    ["h"] = "cycle",
    ["H"] = "cycle sub down",
    ["v"] = "cycle sub visibility",

    -- Pitch shift
    ["f2"] = "af toggle asetrate=44100*1.08",
    ["f3"] = "af toggle asetrate=44100*1.12",
    ["f4"] = "af toggle asetrate=44100*1.",
    ["f5"] = "af toggle asetrate=44100*1.32",
    ["f6"] = "af toggle asetrate=44100*1.52",

    -- Bass Boost
    ["f1"] = "af toggle superequalizer=2b=3:3b=1:4b=2",

    -- Window Manipulation
    ["="] = "add video-zoom 0.1",
    ["-"] = "add video-zoom -0.1",
    ["alt+up"] = "add video-pan-y 0.1",
    ["alt+down"] = "add video-pan-y -0.1",
    ["alt+left"] = "add video-pan-x 0.1",
    ["alt+right"] = "add video-pan-x -0.1",
    ["e"] = "cycle_values video-rotate 90 180 270 0",
    ["r"] = "set video-zoom 0 ; set video-pan-x 0 ; set video-pan-y 0",
}

local reply = function()
    for key, command in pairs(key) do
        local options = {}
        if key == "left" or key == "right" then
            options = { "repeatable" }
        end

        mp.add_forced_key_binding(key, function()
            mp.command(command)
        end, unpack(options))
    end
end

return reply()
