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
alias dotfiles="cd ${REPOS}/dotfiles"


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
