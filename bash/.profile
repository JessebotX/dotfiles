# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "${BASH_VERSION}" ]; then
	# include .bashrc if it exists
	if [ -f "${HOME}/.bashrc" ]; then
		. "${HOME}/.bashrc"
	fi
fi

if [ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile.sh" ]; then
	. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/profile.sh"
fi

if [ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/local-profile.sh" ]; then
	. "${XDG_CONFIG_HOME:-$HOME/.config}/shell/local-profile.sh"
fi

### End
# vim: ts=8 sw=8 sts=8 noet
