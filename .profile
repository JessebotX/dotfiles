# if running bash
if [ -n "$BASH_VERSION" ]; then
	# include .bashrc if it exists
	if [ -f "$HOME/.bashrc" ]; then
		. "$HOME/.bashrc"
	fi
fi

# profiles/envvars/aliases
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile" ] && \
  	. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && \
	. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/machinerc" ] && \
  	. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/machinerc"
