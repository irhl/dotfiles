local mp = require 'mp'

local opts = {
  -- Audio Settings
  volume           = '50',
  volume_max       = '100',
  audio_device     = 'alsa/plug:dmix',
  ao               = 'alsa',
  af               = 'superequalizer=2b=3:3b=1:4b=2',

  -- Video Settings
  interpolation    = 'no',
  deband           = 'no',
  hwdec            = 'no',
  hdr_compute_peak = 'no',
  video_sync       = 'audio',
  vo_image_format  = 'png',
  vo               = 'gpu',
  vf               = 'format=colorlevels=full:colormatrix=auto',

  -- Cache Tuning
  cache                  = 'yes',
  demuxer_max_bytes      = '20M',
  demuxer_max_back_bytes = '20M',

  -- Runtime Behavior
  loop_file              = 'yes',
  ontop                  = 'yes',
  window_dragging        = 'no',
  hidpi_window_scale     = 'no',
  scaler_resizes_only    = 'no',
  input_default_bindings = 'no',
  input_builtin_bindings = 'no',

  -- Player Settings
  alpha                     = 'no',
  background                = '#f2e4d5',
  geometry                  = '20%',
  video_margin_ratio_top    = '0.04',
  video_margin_ratio_left   = '0.04',
  video_margin_ratio_right  = '0.04',
  video_margin_ratio_bottom = '0.04',

  -- Caption Settings
  sub_color        = '#edcd7d',
  sub_font         = 'Undefined-Medium',
  sub_font_size    = '50',
  sub_spacing      = '0.3',
  sub_pos          = '90',
  slang            = 'nl,jp,jpn,en,eng',
  alang            = 'nl,jp,jpn.en,eng',
  osc              = 'no',
  osd_level        = '0',
  term_status_msg  = "${time-pos} / ${duration}"
}

local reply = function()
  for opt, arg in pairs(opts) do
    mp.set_property('options/' .. opt:gsub('_', '-'), arg)
  end
end

return reply ()
