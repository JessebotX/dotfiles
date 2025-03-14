;;; my-config-module-keybindings.el -*- lexical-binding: t; -*-
;; Copyright (c) 2025.

;;; Commentary:
;; Keymap features

;;; Code:

(defcustom my/define-leader-key-prefix "C-c"
  "Prefix for personal keychord mappings.")

(keymap-global-set my/define-leader-key-prefix nil)

(defun my/define-leader-key (key action)
  (keymap-global-set (concat my/define-leader-key-prefix " " key) action))

(use-package which-key
  :ensure nil
  :hook (emacs-startup . which-key-mode)
  :custom
  (which-key-idle-delay 0.1))

;;; General default keybindings

(global-set-key [remap list-buffers] 'ibuffer)
(keymap-global-set "<escape>" 'keyboard-escape-quit)
(keymap-global-set "C--" 'text-scale-decrease)
(keymap-global-set "C-=" 'text-scale-increase)
(keymap-global-set "C-+" 'text-scale-increase)
(keymap-global-set "M-]" 'forward-paragraph)
(keymap-global-set "M-[" 'backward-paragraph)

;; End
(provide 'my-config-module-keybindings)
