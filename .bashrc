case $- in
	*i*) ;; # interactive
	*) return ;; 
esac

# change history file location
export HISTFILE="$XDG_CACHE_HOME/bash/history"

# [user@hostname CURRENTDIR]$ 
#export PS1="\e[1m\[\e[35m\][\[\e[31m\]\u\[\e[34m\]@\h \[\e[32m\]\W\[\e[35m\]]$\e[0m\[\e[39m\] "
#export PS1="\[$(tput bold)\]\[\033[38;5;13m\][\[$(tput sgr0)\]\[\033[38;5;1m\]\u\[$(tput sgr0)\]\[\033[38;5;4m\]@\[$(tput sgr0)\]\[\033[38;5;41m\]\h\[$(tput sgr0)\] \[$(tput sgr0)\]\[$(tput bold)\]\[\033[38;5;3m\]\W\[$(tput sgr0)\]\[\033[38;5;13m\]]\\$\[$(tput sgr0)\]"

# Draw prompt
export PS1="\[\033[01;35m\]"
export PS1=$PS1"["
export PS1=$PS1"\[\033[01;31m\]"
export PS1=$PS1"\u"
export PS1=$PS1"\[\033[01;34m\]"
export PS1=$PS1"@"
export PS1=$PS1"\h "
export PS1=$PS1"\[\033[01;32m\]"
export PS1=$PS1"\W"
export PS1=$PS1"\[\033[01;35m\]"
export PS1=$PS1"]$ "
export PS1=$PS1"\[\033[00;39m\]"

bind 'set show-all-if-ambiguous on'
bind 'set completion-ignore-case on'
bind 'set show-mode-in-prompt on'
bind 'set vi-cmd-mode-string "\1\e[2 q\2"'
bind 'set vi-ins-mode-string "\1\e[6 q\2"'

set -o vi

[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/machinerc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/machinerc"

