local key = {
  q = "quit",
  z = "playlist-next",
  Z = "playlist-prev",
  x = "add volume 15",
  X = "add volume -15",
  m = "cycle mute",
  left  = "seek -10",
  right = "seek 10",
  space = "cycle pause",

  c = "cycle sub",
  C = "cycle sub down",
  v = "cycle sub visibility",

  h = "add video-pan-x -0.1",
  j = "add video-pan-y -0.1",
  k = "add video-pan-y 0.1",
  l = "add video-pan-x 0.1",
  f = "add video-zoom 0.1",
  F = "add video-zoom -0.1",
  r = "set video-zoom 0 ; " ..
      "set video-pan-x 0 ; " ..
      "set video-pan-y 0",

  e = "cycle_values video-rotate 90 180 270 0",

  f1 = "af toggle superequalizer=2b=3:3b=1:4b=2",
  f2 = "af toggle asetrate=44100*1.08",
  f3 = "af toggle asetrate=44100*1.12",
  f4 = "af toggle asetrate=44100*1.22",
  f5 = "af toggle asetrate=44100*1.32",
  f6 = "af toggle asetrate=44100*1.52",
}

local reply = function()
    for key, action in pairs(key) do
        local options = {}
        if key == "left" or key == "right" then
            options = { "repeatable" }
        end

        mp.add_forced_key_binding(key, function()
            mp.command(action)
        end, unpack(options))
    end
end

return reply()
