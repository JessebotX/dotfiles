;;; my-config-module-defaults.el -*- lexical-binding: t; -*-

;; Copyright (c) 2025.

;;; Commentary:
;; Base Emacs configurations to make Emacs bearable.

;;; Code:

(defcustom my/defaults-indent-width 4
  "Width of tab stops, in columns. Controls `tab-width'."
  :type '(natnum))

(defcustom my/defaults-indent-expand-tabs t
  "If non-nil, indentation will insert spaces instead of a TAB
character.

Controls `indent-tabs-mode'."
  :type '(boolean))

;; Indentation
(setq-default indent-tabs-mode (not my/defaults-indent-expand-tabs))
(setq-default tab-width my/defaults-indent-width)

;; Line numbers
(setopt display-line-numbers-type 'relative)
(setopt display-line-numbers-width 4)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Better scrolling
(setq auto-window-vscroll nil)
(setopt scroll-conservatively 100)
(setopt fast-but-imprecise-scrolling t)
(setopt scroll-preserve-screen-position t)
(setopt jit-lock-defer-time 0)

;; Better buffer search
(setopt isearch-lazy-count t)
(setopt lazy-count-prefix-format nil)
(setopt lazy-count-suffix-format "   (%s/%s)")

;; Whitespace
(setopt whitespace-display-mappings '((tab-mark 9 [#x21e5 9] [92 9])))
(setopt whitespace-style '(face tabs tab-mark trailing))

(add-hook 'text-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)

;; Deleting text
(setopt backward-delete-char-untabify-method 'hungry)

(delete-selection-mode 1)

;; Auto-update changed buffers
(setopt global-auto-revert-non-file-buffers t)

(global-auto-revert-mode 1)

;; Misc. files
(setopt create-lockfiles nil)
(setopt make-backup-files nil)

;; Visual/audible bells
(setopt ring-bell-function #'ignore)
(setopt visible-bell nil)

;; Better window splits
(advice-add #'split-window-below :after (lambda (&rest _) (other-window 1)))
(advice-add #'split-window-right :after (lambda (&rest _) (other-window 1)))

;; Unique buffer naming
(setopt uniquify-buffer-name-style 'forward)
(setopt uniquify-ignore-buffers-re "^\\*")
(setopt uniquify-separator "/")

;; Misc.
(set-default-coding-systems 'utf-8)

(setq-default compile-command "make")
(setq custom-file (expand-file-name (convert-standard-filename "etc/custom.el") user-emacs-directory))

(setopt ad-redefinition-action 'accept)
(setopt warning-minimum-level :error)
(setopt use-short-answers t)

(blink-cursor-mode -1)
(global-so-long-mode 1)
(recentf-mode 1)
(winner-mode 1) ; Save window layouts

(global-set-key [remap list-buffers] 'ibuffer)
(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-global-set "C--" 'text-scale-decrease)
(keymap-global-set "C-=" 'text-scale-increase)
(keymap-global-set "C-+" 'text-scale-increase)
(keymap-global-set "M-]" 'forward-paragraph)
(keymap-global-set "M-[" 'backward-paragraph)
(keymap-global-set "C-c SPC" 'just-one-space)

;;; End
(provide 'my-config-module-defaults)
