#!/bin/sh

if [ "$TERM" = linux ]; then
    export PS1='-> '
    export ENV="$HOME/.profile"
    export PATH="$HOME/.local/share/bin:$PATH"
    export PATH="$PATH:$HOME/.cargo/bin"
    export CFLAGS="-O3 -march=native -pipe"
    export CXXFLAGS="$CFLAGS"
    export MAKEFLAGS=-j8
    export LANG=en_US.UTF-8
    export LESSHISTFILE=-
    export WLR_DRM_NO_MODIFIERS=1
    export WLR_DRM_DEVICES=/dev/dri/card0
    export XDG_RUNTIME_DIR="/tmp/1000"

    mkdir -p "${XDG_RUNTIME_DIR}"
fi

unset MAIL
unset LANGUAGE
unset LS_COLORS
unset LC_COLLATE
unset MOTD_SHOWN
unset MOZ_PLUGIN_PATH

function fd { df -h; }
function fu { ls -l /dev/disk/by-uuid/; }
function fv { fuser -fv /dev/snd/* /dev/dsp*; }

function hwhl { ssu -- hwhl; }
function make { make CC=clang; }
function wget { wget --no-hsts; }
function x0 { curl -F "file=@$1" 'https://x0.at'; }
function cv { grim -g "$(slurp -p)" -t ppm - |
    convert - -format '%[pixel:p{0,0}]' txt:-
}
