# change history file location
export HISTFILE="$XDG_CACHE_HOME/bash/history"

# Draw prompt
export PS1="\[\033[01;35m\]"		# magenta bold
export PS1=$PS1"["					# [
export PS1=$PS1"\[\033[01;31m\]"	# red bold
export PS1=$PS1"$USER"					# username (ie. user)
export PS1=$PS1"\[\033[01;34m\]"	# blue bold
export PS1=$PS1"@"					# @
export PS1=$PS1"\h "					# hostname
export PS1=$PS1"\[\033[01;32m\]"	# green bold
export PS1=$PS1"\W"					# current working directory basename
export PS1=$PS1"\[\033[01;35m\]" # magenta bold
export PS1=$PS1"]$ "					# ]
export PS1=$PS1"\[\033[00m\]"	# default regular

bind 'set show-all-if-ambiguous on'
bind 'set completion-ignore-case on'
bind 'set show-mode-in-prompt on'
bind 'set vi-cmd-mode-string "\1\e[2 q\2"'
bind 'set vi-ins-mode-string "\1\e[2 q\2"'
bind 'TAB:menu-complete'

set -o vi

# profiles/envvars/aliases
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile" ] && \
  	. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && \
	. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/machinerc" ] && \
  	. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/machinerc"
