alias ls="ls --color=auto"
alias ll="ls -lA --color=auto"
alias dir="ls -lA --color=auto"
alias vi="${EDITOR}"
alias py="source .venv/bin/activate"

# bookmarks
alias "cd.."="cd .."
alias r="cd ${REPOS}"
alias repos="cd ${REPOS}"
alias dot="cd ${REPOS}/dotfiles"
alias dots="cd ${REPOS}/dotfiles"
alias dotfiles="cd ${REPOS}/dotfiles"
alias s="cd ${HOME}/Sync"
alias sync="cd ${HOME}/Sync"
alias w="cd ${HOME}/Sync/work"

# bookworm
alias bookworm="com.github.babluboy.bookworm"

# fastfetch
if [ "$(command -v fastfetch)" ]; then
	alias neofetch="fastfetch"
fi

# exa
if [ "$(command -v exa)" ]; then
	alias exa="exa --icons -a"
	alias ll="exa --icons --long -a"
	alias dir="exa --icons --long -a"
fi
