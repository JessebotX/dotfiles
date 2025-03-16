;;; init.el -*- lexical-binding: t; -*-
;; Copyright (c) 2025.
;; SPDX-License-Identifier: 0BSD

;;; Commentary:
;; Main emacs configuration file. See `early-init.el' for
;; configurations that happen before emacs startup.

;;; Code:

;;; Variables

(defvar my/leader-key "C-c")
(defvar my/lisp-modules-dirs '("lisp" "modules"))
(defvar my/package-etc-directory (locate-user-emacs-file "etc"))
(defvar my/package-var-directory (locate-user-emacs-file "var"))

;;; General settings
;;;; Load path
(dolist (dir my/lisp-modules-dirs)
  (add-to-list 'load-path (locate-user-emacs-file dir)))

;;;; Commands
(defun my/set-theme (theme)
  "Set emacs current color theme to THEME.

THEME is a string matching its symbol name.
e.g. \"tango-dark\" => 'tango-dark"
  (interactive (list (completing-read "Set theme: " (custom-available-themes))))
  (mapc 'disable-theme custom-enabled-themes)
  (load-theme (intern theme) t)
  (enable-theme (intern theme)))

(defun my/kill-ring-clear ()
  "Clear Emacs copy-paste clipboard (i.e. `kill-ring')."
  (interactive)
  (setq kill-ring nil)
  (garbage-collect))

;;; Elisp package management

(require 'my-config-elpaca-pkgm)

;;; Config modules
;; NOTE: Modules under (locate-user-emacs-file "modules/")

;; Base emacs configurations
(use-package my-config-module-defaults
  :ensure nil
  :custom
  (my/defaults-indent-width 4)
  (my/defaults-indent-expand-tabs t))

(use-package my-config-module-keybindings
  :ensure nil
  :config
  (my/define-leader-key "SPC" 'just-one-space))

;; Emacs themes, fonts, and other UI packages
(use-package my-config-module-ui
  :ensure nil
  :custom
  (my/ui-font-size-default 12)
  (fontaine-latest-state-file (expand-file-name "fontaine-latest-state.eld" my/package-var-directory))
  :config
  (with-eval-after-load 'modus-themes
    (my/set-theme "modus-operandi")))

;; Minibuffer and completion packages
(use-package my-config-module-minibuffer
  :ensure nil
  :config
  (with-eval-after-load 'consult
    (my/define-leader-key "r r" 'consult-recent-file)
    (my/define-leader-key "r b" 'consult-bookmark)
    (my/define-leader-key "s s" 'consult-line)

    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap bookmark-jump] 'consult-bookmark))

  (with-eval-after-load 'vertico
    (let ((map vertico-map))
      (keymap-set map "C-j" 'vertico-next)
      (keymap-set map "C-k" 'vertico-previous)
      (keymap-set map "C-M-j" 'vertico-next-group)
      (keymap-set map "C-M-k" 'vertico-previous-group))))

;; Text editing packages
(use-package my-config-module-editing
  :ensure nil
  :config
  (keymap-global-set "M-;" 'evilnc-comment-or-uncomment-lines))

;; Text/programming languages
(use-package my-config-module-langs
  :ensure nil)

;;; Other emacs packages
(use-package my-config-module-apps
  :ensure nil
  :config
  (global-set-key [remap other-window] 'ace-window))

;;; TODO move to modules
(defun my/font-size-increase ()
  (interactive)
  (let ((font-size (+ (face-attribute 'default :height) 10)))
    (message (format "Font size: %d" font-size))
    (custom-set-faces
     `(default ((t :height ,font-size))))))

(keymap-global-set "C-=" #'my/font-size-increase)
(keymap-global-set "C-+" #'my/font-size-increase)

(defun my/font-size-decrease ()
  (interactive)
  (let ((font-size (- (face-attribute 'default :height) 10)))
    (message (format "Font size: %d" font-size))
    (custom-set-faces
     `(default ((t :height ,font-size))))))

(keymap-global-set "C--" #'my/font-size-decrease)

(defun my/font-size-reset ()
  (interactive)
  (let ((font-size (* my/ui-font-size-default 10)))
    (message (format "Font size: %d" font-size))
    (custom-set-faces
     `(default ((t :height ,font-size))))))

(keymap-global-set "C-0" #'my/font-size-reset)

;;; End
(load (locate-user-emacs-file "machine-init.el") :noerror :nomessage)
