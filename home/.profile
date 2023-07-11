export PS1='-> '
export ENV=$HOME/.daisy
export PATH=$HOME/.local/share/bin:$PATH
export PATH=$PATH:$HOME/.cargo/bin
export LESSHISTFILE=-
export WLR_DRM_NO_MODIFIERS=1
export WLR_DRM_DEVICES=/dev/dri/card0
export XDG_RUNTIME_DIR=/tmp/${UID}-runtime-dir

## FILL EMPTY WAYLAND SOCKET
if [ ! -d "${XDG_RUNTIME_DIR}" ]; then
    mkdir "${XDG_RUNTIME_DIR}"
    chmod 0700 "${XDG_RUNTIME_DIR}"
fi

## MY EARPHONES ARE NOT LOUD
amixer sset 'Master' 100%
