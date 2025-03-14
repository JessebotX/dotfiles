## NOTE: aliases.sh depends on profile.sh loading first

### General
# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
alias ?='search' # script that looks things up using lynx, etc.
alias cd..='cd ..'
alias r="cd ${USER_REPOS}"
alias sy="cd ${USER_SYNC}"
alias dots="cd ${USER_DOTFILES}"

### ls

if [ -x '/usr/bin/dircolors' ]; then
	test -r '~/.dircolors' && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
	alias ls='ls --color=auto'
	alias dir='dir --color=auto'
	alias vdir='vdir --color=auto'

	alias grep='grep --color=auto'
	alias fgrep='fgrep --color=auto'
	alias egrep='egrep --color=auto'
fi

### eza (ls replacement)

if [ -n "$(command -v eza)" ]; then
	alias l="eza -A --icons"
	alias ls="eza -A --icons"
	alias ll="eza -lA --icons"
	alias dir="eza -lA --icons"
	alias tree="eza --icons --tree"
fi

### Neovim

if [ -n "$(command -v nvim)" ]; then
	alias vi="nvim"
	alias vim="nvim"
fi

### xdg-open

if [ -n "$(command -v xdg-open)" ]; then
	alias o="xdg-open"
fi

### wget

alias wget="wget --hsts-file=${XDG_CACHE_HOME:-${HOME}/.cache}/wget-hsts"

# vim: ts=8 sw=8 noet
