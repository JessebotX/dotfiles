export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"

export TERMINAL="alacritty"
export EDITOR="vim"
export REPOS="${HOME}/Repos"
export USER_SCRIPTS="${REPOS}/bin"

export PATH="${USER_SCRIPTS}:${HOME}/.local/bin:${HOME}/bin:${PATH}"

### nix
if [ -d "~/.nix-profile" ]; then
	. ~/.nix-profile/etc/profile.d/nix.sh
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
export PATH="${PATH}:/usr/local/go/bin"

### lynx
export LYNX_CFG_PATH="$XDG_CONFIG_HOME"/lynx.cfg

### nuget
export NUGET_PACKAGES="$XDG_CACHE_HOME"/NuGetPackages

### wget
export WGETRC="$XDG_CONFIG_HOME/wgetrc"
