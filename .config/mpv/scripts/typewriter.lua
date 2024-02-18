local mp = require('mp')

function main()
    local output = os.getenv('HOME') .. '/.config/mpv/typewriter.csv'
    local size_limit = 10 * 1024 * 1024 -- 10mb

    local file = io.open(output, 'rb')
    if file then
        local size = file:seek('end')
        file:close()

        if size > size_limit then
            file = io.open(output, 'w')
            file:close()
        end
    end

    local file = io.open(output, 'a')
    if file then
        local current = os.date('[%Y-%m-%d_%H:%M:%S]')
        local metadata = mp.get_property_native('metadata')
        file:write('\n' .. current .. '\n')

        if metadata then
            for key, value in pairs(metadata) do
                file:write(key .. ': ' .. value .. '\n')
            end
        end

        file:close()
    end
end

mp.register_event('file-loaded', main)
