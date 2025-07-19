### General
export EDITOR="nvim"

#### XDG base directories
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"

#### Local scripts bin
export USER_APPLICATIONS="${HOME}/bin"
export USER_SYNC="${HOME}/Sync"
export USER_REPOS="${HOME}/src"
export USER_SCRIPTS="${USER_REPOS}/bin"
export USER_DOTFILES="${USER_REPOS}/dotfiles"

export PATH="${PATH}:${HOME}/.local/bin"
export PATH="${PATH}:${USER_SCRIPTS}"
export PATH="${PATH}:${USER_APPLICATIONS}"

### GCC
# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

### Go programming language

export GOPATH="${XDG_DATA_HOME}/go"
export GOMODCACHE="${XDG_CACHE_HOME}/go/mod"
export PATH="${PATH}:/usr/local/go/bin:${GOPATH}/bin"

### GTK 2
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc":"$XDG_CONFIG_HOME/gtk-2.0/gtkrc.mine"

### Lynx browser

export LYNX_CFG="${XDG_CONFIG_HOME}/lynx/lynx.cfg"

### python
export PYTHON_HISTORY="${XDG_STATE_HOME}/python_history"
export PYTHONPYCACHEPREFIX="${XDG_CACHE_HOME}/python"
export PYTHONUSERBASE="${XDG_DATA_HOME}/python"

### Rust programming language

export CARGO_HOME="${XDG_DATA_HOME}/cargo" 
export RUSTUP_HOME="${XDG_DATA_HOME}/rustup" 

if [ -f "${CARGO_HOME}/env" ]; then
	. "${CARGO_HOME}/env"
fi

### yazi
export PATH="${PATH}:${USER_APPLICATIONS}/yazi-x86_64-unknown-linux-gnu"

y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	yazi "$@" --cwd-file="$tmp"
	IFS= read -r -d '' cwd < "$tmp"
	[ -n "$cwd" ] && [ "$cwd" != "$PWD" ] && builtin cd -- "$cwd" || exit
	rm -f -- "$tmp"
}

### w3m
export W3M_DIR="${XDG_STATE_HOME}/w3m"

### wget
export WGETRC="${XDG_CONFIG_HOME}/wgetrc"
