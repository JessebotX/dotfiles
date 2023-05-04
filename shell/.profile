# if running bash
if [ -n "$BASH_VERSION" ]; then
	# include .bashrc if it exists
	if [ -f "$HOME/.bashrc" ]; then
		. "$HOME/.bashrc"
	fi
else
	# profiles/envvars/aliases
	[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile.sh" ] && \
		. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile.sh"
	[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliases.sh" ] && \
		. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliases.sh"

	# Draw prompt
	export PS1="\033[01;35m"     # magenta bold
	export PS1=$PS1"["           # [
	export PS1=$PS1"\033[01;31m" # red bold
	export PS1=$PS1"$USER"       # username (ie. user)
	export PS1=$PS1"\033[01;34m" # blue bold
	export PS1=$PS1"@"           # @
	export PS1=$PS1"\h "         # hostname
	export PS1=$PS1"\033[01;32m" # green bold
	export PS1=$PS1"\W"          # current working directory basename
	export PS1=$PS1"\033[01;35m" # magenta bold
	export PS1=$PS1"]$ "         # ]
	export PS1=$PS1"\033[00m"    # default regular
fi

if [ -e /home/user/.nix-profile/etc/profile.d/nix.sh ]; then . /home/user/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
