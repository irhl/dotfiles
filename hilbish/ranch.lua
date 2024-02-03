local n = {}
local quotes = require('quotes')

n.n1 = {
    '~/.local/share/bin/',
}

n.n2 = {
    '~/.cargo/bin/',
}

n.n3 = {
    MAKEFLAGS    = '-j8',
    CFLAGS       = '-O3 -march=native -pipe',
    CXXFLAGS     = os.getenv('CFLAGS') or '',
    LANG         = 'en_US.UTF-8',
    LESSHISTFILE = '-',

    WLR_DRM_NO_MODIFIERS = '1',
    WLR_DRM_DEVICES      = '/dev/dri/card0'
}

n.n4 = {
    wget = '--no-hsts',
    make = 'make CC=gcc',

    s1 = 'curl -F "file=@hi.txt" "https://x0.at"',
    s2 = 'grim -g "$(slurp -p)" -t ppm - | ' ..
         'convert - -format "%[pixel:p{0,0}]" txt:-',

    a1 = 'df -h',
    a2 = 'df -h | grep "nvme"',
    a3 = 'ls -l /dev/disk/by-uuid/',
    a4 = 'lsusb',

    b1 = 'print("' .. quotes.villager_chant .. '")',
}

n.n5 = {
    autocd   = true,
    motd     = false,
    greeting = false,
}

return n
