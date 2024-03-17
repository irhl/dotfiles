local M = {}

M.keymaps = {
  q = 'quit',
  z = 'playlist-next',
  Z = 'playlist-prev',
  x = 'add volume 5',
  X = 'add volume -10',
  m = 'cycle mute',
  left  = 'seek -10',
  right = 'seek 10',
  space = 'cycle pause',

  c = 'cycle sub',
  C = 'cycle sub down',
  v = 'cycle sub visibility',

  h = 'add video-pan-y -0.1',
  j = 'add video-pan-y  0.1',
  k = 'add video-pan-x -0.1',
  l = 'add video-pan-x 0.1',
  f = 'add video-zoom 0.1',
  F = 'add video-zoom -0.1',
  r = 'set video-zoom 0 ; ' ..
      'set video-pan-x 0 ; ' ..
      'set video-pan-y 0',
  e = 'cycle_values video-rotate 90 180 270 0',
  ['/'] = 'vf toggle hflip',

  f1 = 'af toggle superequalizer=2b=3:3b=1:4b=2',
  f2 = 'af toggle asetrate=44100*1.08',
  f3 = 'af toggle asetrate=44100*1.12',
  f4 = 'af toggle asetrate=44100*1.22',
  f5 = 'af toggle asetrate=44100*1.32',
  f6 = 'af toggle asetrate=44100*1.52',
}

M.options = {
  volume = '60',
  volume_max = '100',
  ao = 'alsa',
  audio_device = 'alsa/plug:dmix',
  af = 'superequalizer=2b=3:3b=1:4b=2',

  vo = 'gpu',
  video_sync = 'audio',
  cache = 'yes',
  demuxer_max_bytes = '20M',
  demuxer_max_back_bytes = '20M',

  loop_file = 'yes',
  input_default_bindings = 'no',
  input_builtin_bindings = 'no',

  alpha = 'no',
  background = '#fcf2e8',
  geometry = '15%',
  ontop = 'yes',
  window_dragging = 'yes',

  osc = 'no',
  osd_level = '0',
  osd_align_y = 'top',

  slang = 'nl,jp,jpn,en,eng',
  alang = 'nl,jp,jpn.en,eng',

  -- image quality
  scale = 'ewa_lanczos',
  cscale = 'ewa_lanczos',
  dscale = 'lanczos',
  dither_depth = 'auto',
  correct_downscaling = 'yes',
  linear_downscaling = 'yes',
  sigmoid_upscaling = 'yes',
}

return M
