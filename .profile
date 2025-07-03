#!/bin/sh

export PS1="-> "
export CFLAGS="-O3 -pipe -march=native"
export CXXFLAGS="$CFLAGS"
export MAKEFLAGS="-j$(nproc)"
export LESSHISTFILE=-
export HISTFILESIZE=""
export HISTSIZE=""
export ENV="/home/irhl/.daisy"
export PATH="$HOME/.local/share/bin:$PATH"

export XDG_RUNTIME_DIR="/tmp/1000"
mkdir -p "${XDG_RUNTIME_DIR}"
