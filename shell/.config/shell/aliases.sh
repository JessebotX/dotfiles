alias ls="ls --color=auto"
alias dir="ls -lA --color=auto"
alias vi="${EDITOR}"

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

if [ "$(command -v gvim)" ]; then
	alias vim="gvim -v"
fi
