config = {
    build = {
        env = {
            CFLAGS = '-O3 -march=native -pipe',
            CXXFLAGS = os.getenv('CFLAGS') or '',
            MAKEFLAGS = '-j8',
            LESSHISTFILE = '-'
        },
        path = {
            prepend = {
                '~/.local/share/bin/',
            },
            append = {
                '~/.cargo/bin/',
            }
        }
    },
    alias = {
        wget = '--no-hsts',
        make = 'make CC=clang',
        hwhl = 'ssu -- hwhl',
        fd = 'df -h',
        fu = 'ls -l /dev/disk/by-uuid/',
        fv = 'fuser -fv /dev/snd/* /dev/dsp*',
        fc = 'grim -g "$(slurp -p)" -t ppm - |'
	.. 'convert - -format "%[pixel:p{0,0}]" txt:-',
        q = 'exit',
    }
}

function macro(config, section)
    if section == "build" then
        local build = config.build
        for key, v in pairs(build.env) do
            os.setenv(key, v)
        end

        local path = build.path
        if path then
            for _, p in ipairs(path.prepend) do
                hilbish.prependPath(p)
            end

            for _, p in ipairs(path.append) do
                hilbish.appendPath(p)
            end
        end

    elseif section == "alias" then
        for alias, cmd in pairs(config.alias) do
            hilbish.alias(alias, cmd)
        end
    end
end

macro(config, "build")
macro(config, "alias")
