nmap j gj
nmap k gk
nmap $ g$
nmap 0 g0
nmap dd g0dg$
nmap yy g0yg$

vmap j gj
vmap k gk
vmap $ g$
vmap 0 g0
vmap dd g0dg$
vmap yy g0yg$

setlocal nonu nornu
setlocal colorcolumn=


if (isdirectory(expand("~/.vim/pack"))) 
	function! s:goyo_enter()
		" something to do with tmux
		if executable('tmux') && strlen($TMUX)
			silent !tmux set status off
			silent !tmux list-panes -F '\#F' | grep -q Z || tmux resize-pane -Z
		endif
		Limelight " focus (limelight.vim)
		setlocal scrolloff=999 " typewriter style centered cursor
		setlocal noshowmode " don't show mode
	endfunction

	" commands to call after exiting Goyo mode
	function! s:goyo_leave()
		" something to do with tmux
		if executable('tmux') && strlen($TMUX)
			silent !tmux set status on
			silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
		endif
		Limelight! " turn off limelight
		setlocal scrolloff=5 " set scroll margin to 5
		setlocal showmode " show mode
	endfunction
endif
