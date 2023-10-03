export ENV="$HOME/.daisy"
export PATH="$HOME/.local/share/bin:$PATH"
export PATH="$PATH:$HOME/local/share/cargo/bin"
export CARGO_HOME="${XDG_DATA_HOME:="$HOME/.local/share"}/cargo"
export RUSTUP_HOME="${XDG_DATA_HOME:="$HOME/.local/share"}/rustup"
export LESSHISTFILE=-
export XAUTHORITY=/tmp/Xauthority

amixer sset 'Master' 100% 2>/dev/null
