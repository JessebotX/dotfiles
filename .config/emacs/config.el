;; disable default startpage
(setq inhibit-startup-message t)

;; EMACS BACKUPS
;; ---------
(setq create-lockfiles nil)
(setq make-backup-files nil)

;; USER INTERFACE
;; --------------
;; make ui more minimal
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)

;; add some margins around emacs
(set-fringe-mode 10)
;; disable audible and visible bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(column-number-mode 1)
(global-display-line-numbers-mode 1)

;; EVIL MODE
;; ---------
(require 'evil)
(evil-mode 1)

;; THEMES
;; ------
(load-theme 'modus-vivendi t)

;; MODELINE
;; --------
;; display date on modeline
(setq display-time-day-and-date t)
(display-time-mode 1)

;; KEYBOARD
;; --------

