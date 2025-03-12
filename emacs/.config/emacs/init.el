;;; init.el -*- lexical-binding: t; -*-

;; Copyright (c) 2025.

;; Permission to use, copy, modify, and/or distribute this software
;; for any purpose with or without fee is hereby granted.
;;
;; THE SOFTWARE IS PROVIDED “AS IS” AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
;; OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:
;;
;; Main emacs configuration file. See `early-init.el' for
;; configurations that happen before emacs startup.

;;; Code:

;;; Variables

(defvar my/define-leader-key-prefix "C-c")
(defvar my/lisp-modules-dirs '("lisp" "modules"))
(defvar my/package-etc-directory (locate-user-emacs-file "etc"))
(defvar my/package-var-directory (locate-user-emacs-file "var"))

;;; General settings
;;;; Load path
(dolist (dir my/lisp-modules-dirs)
  (add-to-list 'load-path (locate-user-emacs-file dir)))

;;;; Commands
(defun my/indent-with-spaces (width)
  "Set the local buffer's line indent size to WIDTH and insert as
spaces.

The value of WIDTH should be a positive integer."
  (interactive "nIndent space width: ")
  (setq-local evil-shift-width width)
  (setq-local tab-width width)
  (setq-local indent-tabs-mode nil))

(defun my/indent-with-tabs (width)
  "Set the local buffer's line indent size to be WIDTH and insert as
a tab character.

The value of WIDTH should be a positive integer."
  (interactive "nIndent tab width: ")
  (setq-local evil-shift-width width)
  (setq-local tab-width width)
  (setq-local indent-tabs-mode t))

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

;;; End
(load (locate-user-emacs-file "machine-init.el") :noerror :nomessage)
