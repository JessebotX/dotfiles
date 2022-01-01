# change history file location
export HISTFILE="$XDG_CACHE_HOME/bash/history"

# Draw prompt
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/prompt" ] && \
  	. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/prompt"

bind 'set show-all-if-ambiguous on'
bind 'set completion-ignore-case on'
bind 'set show-mode-in-prompt on'
bind 'set vi-cmd-mode-string ""'
bind 'set vi-ins-mode-string ""'
bind 'TAB:menu-complete'

set -o vi

# profiles/envvars/aliases
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile" ] && \
  	. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && \
	. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/machinerc" ] && \
  	. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/machinerc"
