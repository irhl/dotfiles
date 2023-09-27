local mp = require 'mp'

-- User Defaults
mp.add_forced_key_binding("right",     function() mp.command("seek 5") end)
mp.add_forced_key_binding("left",      function() mp.command("seek -5") end)

mp.add_forced_key_binding("0",         function() mp.command("add volume 2") end)
mp.add_forced_key_binding("9",         function() mp.command("add volume -2") end)

mp.add_forced_key_binding("space",     function() mp.command("cycle pause") end)
mp.add_forced_key_binding("m",         function() mp.command("cycle mute") end)

mp.add_forced_key_binding("q",         function() mp.command("quit") end)

mp.add_forced_key_binding("h",         function() mp.command("cycle sub") end)
mp.add_forced_key_binding("H",         function() mp.command("cycle sub down") end)
mp.add_forced_key_binding("v",         function() mp.command("cycle sub visibility") end)

-- Directory Slide
mp.add_forced_key_binding("a",         function() mp.command("playlist-next") end)
mp.add_forced_key_binding("z",         function() mp.command("playlist-prev") end)

-- Video Scaling
mp.add_forced_key_binding("=",         function() mp.command("add video-zoom 0.1") end)
mp.add_forced_key_binding("-",         function() mp.command("add video-zoom -0.1") end)
mp.add_forced_key_binding("alt+left",  function() mp.command("add video-pan-x 0.1") end)
mp.add_forced_key_binding("alt+right", function() mp.command("add video-pan-x -0.1") end)
mp.add_forced_key_binding("alt+up",    function() mp.command("add video-pan-y 0.1") end)
mp.add_forced_key_binding("alt+down",  function() mp.command("add video-pan-y -0.1") end)

mp.add_forced_key_binding("r", function()
	mp.command("set video-zoom 0 ; set video-pan-x 0 ; set video-pan-y 0")
end)

mp.add_forced_key_binding("e", function()
	mp.command("cycle_values video-rotate 90 180 270 0")
end)

-- Pitch Shift
mp.add_forced_key_binding("f2", function() mp.command("af toggle asetrate=44100*1.08") end)
mp.add_forced_key_binding("f3", function() mp.command("af toggle asetrate=44100*1.12") end)
mp.add_forced_key_binding("f4", function() mp.command("af toggle asetrate=44100*1.22") end)
mp.add_forced_key_binding("f5", function() mp.command("af toggle asetrate=44100*1.32") end)
mp.add_forced_key_binding("f6", function() mp.command("af toggle asetrate=44100*1.52") end)

-- Bass Boost
mp.add_forced_key_binding("f1", function()
	mp.command("af toggle superequalizer=2b=3:3b=1:4b=2")
end)
