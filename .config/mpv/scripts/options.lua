local mp = require 'mp'

local opts = {
  volume = '50',
  volume_max = '100',
  ao = 'alsa',
  audio_device = 'alsa/plug:dmix',
  af = 'superequalizer=2b=3:3b=1:4b=2',

  vo = 'gpu',
  hwdec = 'auto',
  vo_image_format = 'png',
  video_sync = 'audio',
  interpolation = 'no',
  hdr_compute_peak = 'no',

  cache = 'yes',
  demuxer_max_bytes = '20M',
  demuxer_max_back_bytes = '20M',

  loop_file = 'yes',
  ontop = 'yes',
  geometry = '18%',
  window_dragging = 'yes',
  hidpi_window_scale = 'no',
  scaler_resizes_only = 'no',
  input_default_bindings = 'no',
  input_builtin_bindings = 'no',

  slang = 'nl,jp,jpn,en,eng',
  alang = 'nl,jp,jpn.en,eng',
}

local reply = function()
  for opt, arg in pairs(opts) do
    mp.set_property('options/' .. opt:gsub('_', '-'), arg)
  end
end

return reply ()
