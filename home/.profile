#!/bin/sh

export ENV="$HOME/.daisy"
export PATH="$HOME/.local/share/bin:$PATH"
export PATH="$PATH:$HOME/.local/share/cargo/bin"
export CARGO_HOME="${XDG_DATA_HOME:="$HOME/.local/share"}/cargo"
export RUSTUP_HOME="${XDG_DATA_HOME:="$HOME/.local/share"}/rustup"
export LESSHISTFILE=-
export CFLAGS="-O3 -march=native -pipe"
export CXXFLAGS="$CFLAGS"
export MAKEFLAGS=-j8
export WLR_DRM_NO_MODIFIERS=1
export WLR_DRM_DEVICES=/dev/dri/card0
export XDG_RUNTIME_DIR=/tmp/${UID}-runtime-dir

function wget { wget --no-hsts; }
function make { make CC=gcc; }
function hwhl { ssu -- hwhl; }
function fd { df -h; }
function fv { fuser -fv /dev/snd/* /dev/dsp*; }
function fc {
    grim -g "$(slurp -p)" -t ppm - |
    convert - -format '%[pixel:p{0,0}]' txt:-
}

function main {
    if [ ! -d "${XDG_RUNTIME_DIR}" ]; then
        mkdir "${XDG_RUNTIME_DIR}"
        chmod 0700 "${XDG_RUNTIME_DIR}"
    fi

    amixer sset 'Master' 100% 2>/dev/null
}

main
