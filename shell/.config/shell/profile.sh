#######################################################################
#                                                                     #
#  ██╗ ██╗     ██╗  ██╗███████╗██╗     ██╗      ██████╗      ██╗ ██╗  #
# ████████╗    ██║  ██║██╔════╝██║     ██║     ██╔═══██╗    ████████╗ #
# ╚██╔═██╔╝    ███████║█████╗  ██║     ██║     ██║   ██║    ╚██╔═██╔╝ #
# ████████╗    ██╔══██║██╔══╝  ██║     ██║     ██║   ██║    ████████╗ #
# ╚██╔═██╔╝    ██║  ██║███████╗███████╗███████╗╚██████╔╝    ╚██╔═██╔╝ #
#  ╚═╝ ╚═╝     ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝ ╚═════╝      ╚═╝ ╚═╝  #
#######################################################################

### Base
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"
export USER_APPLICATIONS="${HOME}/bin"
export USER_SYNC="${HOME}/Sync"
export USER_SRC="${HOME}/src"
export USER_SCRIPTS="${USER_SRC}/bin"
export USER_DOTFILES="${USER_SRC}/dotfiles"

export EDITOR="nvim"
export PATH="${PATH}:${HOME}/.local/bin"
export PATH="${PATH}:${USER_SCRIPTS}"
export PATH="${PATH}:${USER_APPLICATIONS}"

alias ?='search' # script that looks things up using lynx, etc.
# Add an "alert" alias for long running commands.  Use like so: sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
alias cd..='cd ..'
alias r="cd ${USER_SRC}"
alias s="cd ${USER_SYNC}"
alias sy="cd ${USER_SYNC}"
alias dots="cd ${USER_DOTFILES}"

if [ -x '/usr/bin/dircolors' ]; then
	test -r '~/.dircolors' && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
	alias l='ls -A --color=auto'
	alias ls='ls -A --color=auto'
	alias ll='ls -lAho --color=auto'
	alias dir='dir --color=auto'
	alias vdir='vdir --color=auto'

	alias grep='grep --color=auto'
	alias fgrep='fgrep --color=auto'
	alias egrep='egrep --color=auto'
fi

### cht.sh
if [ -n "$(command -v cht.sh)" ]; then
	alias h="cht.sh"
fi

### eza (ls replacement)
if [ -n "$(command -v eza)" ]; then
	alias l="eza -A --icons"
	alias ls="eza -A --icons"
	alias ll="eza -lA --icons"
	alias dir="eza -lA --icons"
	alias tree="eza --icons --tree"
fi

### GCC
# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

### Go programming language
export GOPATH="${XDG_DATA_HOME}/go"
export GOMODCACHE="${XDG_CACHE_HOME}/go/mod"
export PATH="${PATH}:/usr/local/go/bin:${GOPATH}/bin"

### GTK 2
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc":"$XDG_CONFIG_HOME/gtk-2.0/gtkrc.mine"

### Lynx
export LYNX_CFG="${XDG_CONFIG_HOME}/lynx/lynx.cfg"

### [Neo]vim
if [ -n "$(command -v nvim)" ]; then
	alias vi="nvim"
	alias vim="nvim"
elif [ -n "$(command -v vim)" ]; then
	alias vi="vim"
	alias vim="vim"
fi

### Node.js/npm
export NPM_CONFIG_USERCONFIG="${XDG_CONFIG_HOME}/npm/npmrc"
export NVM_DIR="${XDG_DATA_HOME}/nvm"

### python
export PYTHON_HISTORY="${XDG_STATE_HOME}/python_history"
export PYTHONPYCACHEPREFIX="${XDG_CACHE_HOME}/python"
export PYTHONUSERBASE="${XDG_DATA_HOME}/python"

if [ -n "$(command -v python)" ]; then
	alias pyv="source ./.venv/bin/activate"
	alias zephv="source ${USER_SRC}/zephyrproject/.venv/bin/activate"
fi

### Rust
export CARGO_HOME="${XDG_DATA_HOME}/cargo" 
export RUSTUP_HOME="${XDG_DATA_HOME}/rustup" 

if [ -f "${CARGO_HOME}/env" ]; then
	. "${CARGO_HOME}/env"
fi

### texlive
export TEXMFHOME="${XDG_DATA_HOME}/texmf"
export TEXMFVAR="${XDG_CACHE_HOME}/texlive/texmf-var"
export TEXMFCONFIG="${XDG_CONFIG_HOME}/texlive/texmf-config"

### w3m
export W3M_DIR="${XDG_STATE_HOME}/w3m"

### wget
export WGETRC="${XDG_CONFIG_HOME}/wgetrc"
alias wget="wget --hsts-file=${XDG_CACHE_HOME:-${HOME}/.cache}/wget-hsts"

### xdg-open
if [ -n "$(command -v xdg-open)" ]; then
	alias o="xdg-open"
fi

### yazi
export PATH="${PATH}:${USER_APPLICATIONS}/yazi-x86_64-unknown-linux-gnu"
y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	yazi "$@" --cwd-file="$tmp"
	IFS= read -r -d '' cwd < "$tmp"
	[ -n "$cwd" ] && [ "$cwd" != "$PWD" ] && cd -- "$cwd" || exit
	rm -f -- "$tmp"
}

alias yy="yazi"
alias ld="yazi"
alias lg="y"

# vim: ts=8 sw=8 noet

