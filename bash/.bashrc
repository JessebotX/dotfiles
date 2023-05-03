# .bashrc

PS1="\[\e[01;30m\]"
PS1="${PS1}╔» "
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
PS1="${PS1}╚═"
PS1="${PS1}\[\e[01;30m\]"
PS1="${PS1}$ "
PS1="${PS1}\[\e[00m\]"

source "${HOME}/.config/shell/profile.sh"
source "${HOME}/.config/shell/aliases.sh"

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
bind 'TAB:menu-complete'

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
