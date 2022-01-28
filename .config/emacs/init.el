;; PACKAGE MANAGEMENT
;; ------------------
(setq user-emacs-directory  "~/.cache/emacs")
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; this is where my actual config lives
(load "~/.config/emacs/config")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("9f1d0627e756e58e0263fe3f00b16d8f7b2aca0882faacdc20ddd56a95acb7c2" "835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(package-selected-packages '(no-littering modus-themes doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
