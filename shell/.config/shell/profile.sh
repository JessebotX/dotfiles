### General
export EDITOR="nvim"

#### XDG base directories
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"

#### Local scripts bin
export USER_SYNC="${HOME}/Sync"
export USER_REPOS="${HOME}/src"
export USER_SCRIPTS="${USER_REPOS}/bin"
export USER_DOTFILES="${USER_REPOS}/dotfiles"

export PATH="${PATH}:${HOME}/.local/bin"
export PATH="${PATH}:${USER_SCRIPTS}"

### GCC
# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

### Go programming language

export GOPATH="${XDG_DATA_HOME}/go"
export GOMODCACHE="${XDG_CACHE_HOME}/go/mod"
export PATH="${PATH}:/usr/local/go/bin:${GOPATH}/bin"

### Lynx browser

export LYNX_CFG="${XDG_CONFIG_HOME}/lynx/lynx.cfg"

### Rust programming language

export CARGO_HOME="${XDG_DATA_HOME}/cargo" 
export RUSTUP_HOME="${XDG_DATA_HOME}/rustup" 

if [ -f "${CARGO_HOME}/env" ]; then
	. ${CARGO_HOME}/env
fi

### w3m
export W3M_DIR="${XDG_STATE_HOME}/w3m"

### wget
export WGETRC="${XDG_CONFIG_HOME}/wgetrc"
