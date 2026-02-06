local mp = require("mp")

local has_run = false
local path = os.getenv("HOME") .. "/.config/mpv/script-opts/"

local function run_script(fname)
    if not fname or fname == "" then return end
    dofile(path .. fname)
end

local audio = {
   ".mp3 .opus .flac",
   "typewriter.lua imim.lua"
}

local image = {
   ".jpeg .jpg .png .webp .gif",
   "imim.lua"
}

local function check_match(tbl, ext)
    if #tbl ~= 2 then return end

    local match = false
    for e in tbl[1]:gmatch("%S+") do
        if e == ext then match = true break end
    end
    if not match then return end

    for script in tbl[2]:gmatch("%S+")
        do run_script(script)
    end
end

local function main()
    if has_run then return end
    has_run = true

    local fname = mp.get_property("path")
    if not fname then return end

    local ext = fname:match("^.+(%.[^%.]+)$")
    if not ext then return end

    ext = ext:lower()
    check_match(audio, ext)
    check_match(image, ext)
end

mp.register_event("start-file", main)
