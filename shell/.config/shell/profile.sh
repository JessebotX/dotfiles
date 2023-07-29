export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"

export TERMINAL="alacritty"
export EDITOR="nvim"
export REPOS="${HOME}/Repos"
export USER_SCRIPTS="${REPOS}/bin"

export PATH="${USER_SCRIPTS}:${HOME}/.local/bin:${HOME}/bin:${PATH}"

### nix
if [ -d "~/.nix-profile" ]; then
	. "~/.nix-profile/etc/profile.d/nix.sh"
fi

### fff
export FFF_HIDDEN=1
export FFF_TRASH="${XDG_DATA_HOME:-${HOME}/.local/share}/Trash"
export FFF_FAV1="${REPOS}"
export FFF_FAV2="${USER_SCRIPTS}"
export FFF_FAV3="${HOME}/Pictures/wallpapers"
export FFF_FAV4="${XDG_CONFIG_HOME:-${HOME}/.config}/hypr"
export FFF_FAV5="${XDG_CONFIG_HOME:-${HOME}/.config}"

##### cd-on-exit
fm() {
    fff "$@"
    cd "$(cat "${XDG_CACHE_HOME:=${HOME}/.cache}/fff/.fff_d")"
}

### go
export GOPATH="$XDG_DATA_HOME"/go
export GOMODCACHE="$XDG_CACHE_HOME"/go/mod
export PATH="${PATH}:${GOPATH}/bin:/usr/local/go/bin"

### lynx
export LYNX_CFG_PATH="$XDG_CONFIG_HOME"/lynx.cfg

### nuget
export NUGET_PACKAGES="$XDG_CACHE_HOME"/NuGetPackages

### nvm (node.js version manager)
export NVM_DIR="$HOME/.config/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

### pnpm
export PNPM_HOME="/home/user/.local/share/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

### rust
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup
export CARGO_HOME="$XDG_DATA_HOME"/cargo

[ -f "${CARGO_HOME}"/env ] && . "${CARGO_HOME}"/env

### sqlite3
export SQLITE_HISTORY=$XDG_DATA_HOME/sqlite_history

### wget
export WGETRC="$XDG_CONFIG_HOME/wgetrc"
