local mp = require("mp")
local home = os.getenv("HOME")
local output = home .. "/.config/mpv/typewriter.csv"

function main()
  local file = io.open(output, "a")

  if file then
    local current = os.date("[%Y-%m-%d, %H:%M:%S]")
    local metadata = mp.get_property_native("metadata")
    file:write("\n" .. current .. "\n")

        if metadata then
            for key, value in pairs(metadata) do
                file:write(key .. ": " .. value .. "\n")
            end
        end

        file:close()
    end
end

mp.register_event("file-loaded", main)
