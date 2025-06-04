# ~/.bashrc: executed by bash(1) for non-login shells.

# If not running interactively, don't do anything
case $- in
	*i*) ;;
	*) return;;
esac

### Shared aliases and environment
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ~/.config/shell/profile.sh ]; then
	. ~/.config/shell/profile.sh
fi

if [ -f ~/.config/shell/local-profile.sh ]; then
	. ~/.config/shell/local-profile.sh
fi

if [ -f ~/.config/shell/aliases.sh ]; then
	. ~/.config/shell/aliases.sh
fi

if [ -f ~/.config/shell/local-aliases.sh ]; then
	. ~/.config/shell/local-aliases.sh
fi

set -o vi

bind 'TAB:menu-complete'
bind 'set show-all-if-ambiguous on'
bind 'set completion-ignore-case on'

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
export HISTFILE="${XDG_STATE_HOME}/bash/history"
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

if ! shopt -oq posix; then
	if [ -f /usr/share/bash-completion/bash_completion ]; then
		. /usr/share/bash-completion/bash_completion
	elif [ -f /etc/bash_completion ]; then
		. /etc/bash_completion
	fi
fi

PS1=""
PS1="${PS1}\[\e[34;1m\]"
PS1="${PS1}["
PS1="${PS1}\[\e[32;1m\]"
PS1="${PS1}\u"
PS1="${PS1}@"
PS1="${PS1}\h"
PS1="${PS1}\[\e[34;1m\]"
PS1="${PS1}]"
PS1="${PS1}\n"
PS1="${PS1}├["
PS1="${PS1}\[\e[33;1m\]"
PS1="${PS1}\w"
PS1="${PS1}\[\e[34;1m\]"
PS1="${PS1}]"
PS1="${PS1}\n"
PS1="${PS1}\[\e[34;1m\]"
PS1="${PS1}└$"
PS1="${PS1}\[\e[00;0m\]"
PS1="${PS1} "

### End
# vim: ts=8 sw=8 sts=8 noet
