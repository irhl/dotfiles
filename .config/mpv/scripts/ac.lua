local M = {}

M.keymaps = {
    left  = 'seek -5',
    right = 'seek 5',
    space = 'cycle pause',

    q = 'quit',
    s = 'screenshot video',
    z = 'playlist-next',
    Z = 'playlist-prev',
    x = 'add volume 5',
    X = 'add volume -5',
    c = 'set speed 0.50',
    C = 'set speed 0.80',
    m = 'cycle mute',
    v = 'cycle sub down',
    V = 'cycle sub visibility',
    e = 'cycle_values video-rotate 90 180 270 0',
    ['/, MBTN_RIGHT'] = 'vf toggle format=yuv420p,hflip',

    h = 'add video-pan-x  -0.01',
    j = 'add video-pan-y   0.01',
    k = 'add video-pan-y  -0.01',
    l = 'add video-pan-x   0.01',
    f = 'add video-zoom    0.1',
    F = 'add video-zoom   -0.1',

    f2 = 'af toggle "lavfi=[pan=1c|c0=1*c0+1*c1]"',
    f3 = 'af remove @eq  ; ' ..
         'af remove @dyn ; ' ..
         'af add @eq:equalizer=w=1:t=o:f=55:g=16:r=f64 ; ' ..
         'af add @dyn:dynaudnorm=b=1:m=1',
    f4 = 'af remove asetrate ; af add asetrate=44100*1.06',
    f5 = 'af remove asetrate ; af add asetrate=44100*1.12',
    f6 = 'af remove asetrate ; af add asetrate=44100*1.22',
    f7 = 'af remove asetrate ; af add asetrate=44100*1.32',
    f8 = 'af remove asetrate ; af add asetrate=44100*1.40',
    f9 = 'af remove asetrate ; af add asetrate=44100*1.50',

    r = 'set video-zoom    0.01 ; ' ..
        'set video-pan-x   0 ;    ' ..
        'set video-pan-y   0 ;    ' ..
        'set video-rotate  0 ;    ' ..
        'set speed         1 ;    ' ..
        'vf set "";               ' ..
        'af set ""',
    R = 'set speed         1 ;    ' ..
        'af set ""',

    ['Shift+b'] = item_set_bg,
    ['Shift+Enter'] = item_trash,
}

M.options = {
    ao = 'alsa',
    volume = '60',
    volume_max = '100',

    vo = 'gpu-next',
    hwdec = 'no',
    vd_lavc_dr = 'no',
    hdr_compute_peak = 'no',
    profile = 'fast',

    -- use this if it's too bright.
    vf = "format:gamma=gamma2.2",

    alpha             = 'no',
    background_color  = '#000000',
    video_zoom        = '0.01',
    geometry          = '21%',
    ontop             = 'yes',

    sub_font          = 'azukifontB',
    sub_font_size     = '70',
    sub_color         = '#fcefbd',
    sub_shadow_color  = '#333333',
    sub_shadow_offset = '2',
    sub_border_size   = '4',
    sub_margin_x      = '50',
    sub_margin_y      = '50',

    slang             = 'nl,jp,jpn,en,eng',
    alang             = 'nl,jp,jpn,en,eng',

    osc               = 'no',
    osd_level         = '0',
    osd_align_y       = 'top',

    window_dragging = 'no',
    no_input_cursor = 'no',
    input_default_bindings = 'no',
    input_builtin_bindings = 'no',

    loop_file = 'yes',
    autocreate_playlist = 'same',
}

return M
