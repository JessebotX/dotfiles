# Colors and prompt
autoload -U colors && colors
PS1="%B"                                 # Enable bold
PS1=$PS1"%{$fg[magenta]%}["              # [ (magenta)
PS1=$PS1"%{$fg[red]%}%n"                 # display username in red
PS1=$PS1"%{$fg[blue]%}@%{$fg[blue]%}%M " # display @hostname as blue
PS1=$PS1"%{$fg[green]%}%1~"              # display current working dir as green
PS1=$PS1"%{$fg[magenta]%}]$"             # display ]$ as magenta
PS1=$PS1"%{$reset_color%}%b "            # reset styling

# profiles/envvars/aliases
[ -f "$HOME/.profile" ] && . "$HOME/.profile"
#[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile" ] && \
          #. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile"
#[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && \
	#. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
#[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/machinerc" ] && \
          #. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/machinerc"

# History
HISTSIZE=10000000
SAVEHIST=10000000
HISTFILE="${XDG_CACHE_HOME:-$HOME/.cache}"/zsh/history

# Disable ctrl-s to freeze terminal.
stty stop undef

# Autocompletion
autoload -U compinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' '+m:{A-Z}={a-z}'
zmodload zsh/complist
compinit
_comp_options+=(globdots)	

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
#bindkey -M menuselect 'h' vi-backward-char
#bindkey -M menuselect 'k' vi-up-line-or-history
#bindkey -M menuselect 'l' vi-forward-char
#bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

bindkey '^[[P' delete-char
# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line
