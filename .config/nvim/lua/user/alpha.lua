local status_ok, alpha = pcall(require, "alpha")
if not status_ok then
	return
end

local dashboard = require("alpha.themes.dashboard")
dashboard.section.header.val = {
	[[  _____                               ]],
	[[ |  ___|                              ]],
	[[ | |__   _ __ ___    __ _   ___  ___  ]],
	[[ |  __| | '_ ` _ \  / _` | / __|/ __| ]],
	[[ | |___ | | | | | || (_| || (__ \__ \ ]],
	[[ \____/ |_| |_| |_| \__,_| \___||___/ ]],
}
dashboard.section.buttons.val = {
	dashboard.button("a", "  Open Current Directory", ":e . <CR>"),
	dashboard.button("p", "  Find project", ":Telescope projects <CR>"),
	dashboard.button("t", "  Find text", ":Telescope live_grep <CR>"),
	dashboard.button("c", "  Configuration", ":e ~/.config/nvim/init.lua <CR>"),
	dashboard.button("q", "  Quit Neovim", ":qa<CR>"),
}

dashboard.opts.opts.noautocmd = true
alpha.setup(dashboard.opts)
