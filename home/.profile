export ENV=$HOME/.daisy
export PATH=$HOME/.local/share/bin:$PATH
export PATH=$PATH:$HOME/.cargo/bin
export LESSHISTFILE=-
export WLR_DRM_NO_MODIFIERS=1
export WLR_DRM_DEVICES=/dev/dri/card0
export XDG_RUNTIME_DIR=/tmp/${UID}-runtime-dir

# fill empty wayland socket
if [ ! -d "${XDG_RUNTIME_DIR}" ]; then
    mkdir "${XDG_RUNTIME_DIR}"
    chmod 0700 "${XDG_RUNTIME_DIR}"
fi

# maximize volume
amixer sset 'Master' 100% 2>/dev/null
