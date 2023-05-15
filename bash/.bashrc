# .bashrc

PS1="\[\e[01;30m\]"
PS1="${PS1}═» "
PS1="${PS1}\[\e[01;34m\]"
PS1="${PS1}\w "
PS1="${PS1}\[\e[01;30m\]"
PS1="${PS1}["
PS1="${PS1}\[\e[01;32m\]"
PS1="${PS1}\u"
PS1="${PS1}\[\e[01;30m\]"
PS1="${PS1}@"
PS1="${PS1}\[\e[01;31m\]"
PS1="${PS1}\h"
PS1="${PS1}\[\e[01;30m\]"
PS1="${PS1}]"
PS1="${PS1}\n"
PS1="${PS1}\[\e[01;30m\]"
PS1="${PS1}═"
PS1="${PS1}\[\e[01;30m\]"
PS1="${PS1}$ "
PS1="${PS1}\[\e[00m\]"

[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile.sh" ] && \
	source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile.sh"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliases.sh" ] && \
	source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliases.sh"


#
# Shell options
#
set -o vi
shopt -s checkwinsize
shopt -s expand_aliases
shopt -s globstar
shopt -s dotglob
shopt -s extglob

bind 'set show-all-if-ambiguous on'
bind 'set completion-ignore-case on'
bind 'TAB:menu-complete'

set keymap vi-command "^P": previous-history
set keymap vi-insert "^P": previous-history

stty stop undef # disable control-s accidental terminal stops

#
# History
#

shopt -s histappend

export HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}/bash/history"
export HISTCONTROL=ignoreboth
export HISTSIZE=10000
export HISTFILESIZE=15000

complete -r

#
# ble.sh
#
#[ -f "${XDG_DATA_HOME:-${HOME}/.local/share}/ble.sh/out/ble.sh" ] && \
#	source "${XDG_DATA_HOME:-${HOME}/.local/share}/ble.sh/out/ble.sh"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/user/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/user/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/home/user/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/user/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

