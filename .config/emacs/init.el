(defvar user/pkgs
  '(all-the-icons
    evil
    modus-themes)
  "List of packages ensured")

;; set user emacs directory to safely avoid things possibly going
;; into ~/.emacs.d/
(setq user-emacs-directory "~/.cache/emacs")

;; Initialize package management (package.el)
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(setq package-selected-packages user/pkgs)
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install package)))

(load "~/.config/emacs/config.el")
