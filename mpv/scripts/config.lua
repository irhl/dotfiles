local config = {
    key = {
        q = "quit",
        z = "playlist-next",
        Z = "playlist-prev",
        x = "add volume 15",
        X = "add volume -15",
        m = "cycle mute",
        left = "seek -10",
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
    },
    opt = {
        volume = '50',
        volume_max = '150',
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
    },
}

local reply = function()
    for section, scmd in pairs(config) do
        for key, args in pairs(scmd) do
            local options = {}
            if key == "left" or key == "right" then
                options = { "repeatable" }
            end

            if section == "key" then
                mp.add_forced_key_binding(key, function()
                    mp.command(args)
                end, unpack(options))

            elseif section == "opt" then
                mp.set_property('options/' .. key:gsub('_', '-'), args)
            end
        end
    end
end

return reply()
