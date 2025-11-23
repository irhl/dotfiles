local mp = require 'mp'

function item_trash()
    local path = mp.get_property("path", "")
    local pos = mp.get_property_number("playlist-pos", nil)

    if not path or path == "" or not pos then
        mp.osd_message("no file to remove", 1)
        return
    end

    local success, err = os.remove(path)
    if not success then
        mp.osd_message("file can't be removed: " .. (err or "error unknown"), 3)
        return
    end

    mp.osd_message("file removed: " .. path, 2)
    mp.commandv("playlist-remove", tostring(pos))

    if pos > 0 then
        mp.set_property_number("playlist-pos", pos - 1)
    else
        mp.command("playlist_next")
    end
end

function item_set_bg()
    local path = mp.get_property("path", "")
    if not path or path == "" then
        mp.osd_message("no file loaded", 1)
        return
    end

    local mode = path:lower():find("tile") and "tile" or "fill"
    os.execute(string.format('swaymsg "output * bg %q %s"', path, mode))

    local config_path = os.getenv("HOME") .. "/.config/sway/config"
    local f1 = io.open(config_path, "r")
    if not f1 then
        mp.osd_message("file failed to open", 2)
        return
    end

    local lines = {}
    for line in f1:lines() do
        if line:match("^%s*bg%s+") then
            local indent = line:match("^(%s*)bg") or ""
            line = string.format("%sbg %s %s", indent, path, mode)
        end
        table.insert(lines, line)
    end
    f1:close()

    local f2 = io.open(config_path, "w")
    if not f2 then
        mp.osd_message("file failed to write", 2)
        return
    end

    for _, line in ipairs(lines) do
        f2:write(line, "\n")
    end
    f2:close()

    mp.osd_message("wallpaper set", 1)
end
