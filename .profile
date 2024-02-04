#!/bin/sh

if [ "$TERM" = linux ]; then
    export ENV="$HOME/.profile"
    export PATH="$HOME/.local/share/bin:$PATH"
    export PATH="$PATH:$HOME/.cargo/bin"
    export LESSHISTFILE=-
    export LANG=en_US.UTF-8
    export CFLAGS="-O2 -pipe -march=native"
    export CXXFLAGS="$CFLAGS"
    export MAKEFLAGS="-j$(nproc)"
    export WLR_DRM_NO_MODIFIERS=1
    export WLR_DRM_DEVICES=/dev/dri/card0
    export XDG_RUNTIME_DIR="/tmp/1000"

    mkdir -p "${XDG_RUNTIME_DIR}"
fi

make() { make CC=gcc; }
wget() { wget --no-hsts; }

s1()   { curl -F "file=@$1" 'https://x0.at'; }
s2()   { grim -g "$(slurp -p)" -t ppm - |
convert - -format '%[pixel:p{0,0}]' txt:-; }

1() { df -Th; }
2() { df -h | grep 'nvme'; }
3() { ls -l /dev/disk/by-uuid/; }
4() { lsusb; }

amixer sset 'Master' 70% > /dev/null 2>&1
export PS1='-> '
