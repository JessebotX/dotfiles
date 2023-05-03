alias ls="ls --color=auto"
alias dir="ls -lA --color=auto"
alias vi="${EDITOR}"

# bookmarks
alias "cd.."="cd .."
alias r="cd ${REPOS}"
alias repos="cd ${REPOS}"

# fastfetch
if [ "$(command -v fastfetch)" ]; then
	alias neofetch="fastfetch"
fi
