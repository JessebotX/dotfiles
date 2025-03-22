;;; init.el -*- lexical-binding: t; -*-
;; Copyright (c) 2025.
;; SPDX-License-Identifier: 0BSD

;;; Commentary:
;; Main emacs configuration file. See `early-init.el' for
;; configurations that happen before emacs startup.

;;; Code:

;;; Variables

(defvar my/leader-key "C-c")
(defvar my/lisp-modules-directories '("lisp" "modules"))
(defvar my/package-etc-directory (locate-user-emacs-file "etc"))
(defvar my/package-var-directory (locate-user-emacs-file "var"))
(defvar my/local-lisp-directory (locate-user-emacs-file "lisp"))
(defvar my/local-modules-directory (locate-user-emacs-file "modules"))
(defvar my/home-directory (getenv "HOME"))
(defvar my/src-directory (expand-file-name "src" my/home-directory))
(defvar my/dotfiles-directory (expand-file-name "dotfiles" my/src-directory))
(defvar my/dotfiles-emacs-directory (expand-file-name "emacs/.config/emacs" my/dotfiles-directory))

;;; General settings
(dolist (dir my/lisp-modules-directories)
  (add-to-list 'load-path (locate-user-emacs-file dir)))

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
    (global-set-key [remap bookmark-jump] 'consult-bookmark)

    (defun my/directory-consult-grep ()
      (interactive)
      (consult-grep (expand-file-name default-directory)))
    (my/define-leader-key "r d" #'my/directory-consult-grep)

    (defun my/directory-consult-ripgrep ()
      (interactive)
      (consult-ripgrep (expand-file-name default-directory)))
    (my/define-leader-key "r g" #'my/directory-consult-ripgrep))

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

;;; Other commands
(defun my/font-size-increase ()
  "Increase font size by 10 (ie. font height by 10)."
  (interactive)
  (let ((font-size (+ (face-attribute 'default :height) 10)))
    (message (format "Font size: %d" font-size))
    (custom-set-faces
     `(default ((t :height ,font-size))))))

(keymap-global-set "C-=" #'my/font-size-increase)
(keymap-global-set "C-+" #'my/font-size-increase)

(defun my/font-size-decrease ()
  "Decrease font size by 1 (ie. font height by 10)."
  (interactive)
  (let ((font-size (- (face-attribute 'default :height) 10)))
    (message (format "Font size: %d" font-size))
    (custom-set-faces
     `(default ((t :height ,font-size))))))

(keymap-global-set "C--" #'my/font-size-decrease)

(defun my/font-size-reset ()
  "Reset the font size to `my/ui-font-size-default'"
  (interactive)
  (let ((font-size (* my/ui-font-size-default 10)))
    (message (format "Font size: %d" font-size))
    (custom-set-faces
     `(default ((t :height ,font-size))))))

(defun my/font-size-set (value)
  "Manually input a font size (e.g. 12)"
  (interactive "nEnter font size (e.g. 12): ")
  (let ((font-size (* value 10)))
    (custom-set-faces
     `(default ((t :height ,font-size))))
    (message (format "Font height set to %d" font-size))))

(keymap-global-set "C-0" #'my/font-size-reset)

(defun my/buffer-copy-base-file-name ()
  "Copy the current buffer's file name"
  (interactive)
  (if (buffer-file-name)
      (kill-new
       (concat
        (file-name-base buffer-file-name)
        "."
        (file-name-extension buffer-file-name)))
    (message "Error: (buffer-file-name) returned nil")))

(defun my/buffer-insert-relative-link-to-file (filepath &optional useless)
  "Get a relative link from the file in the current buffer to FILEPATH.

USELESS is not used."
  (interactive (find-file-read-args "Link to file: " (confirm-nonexistent-file-or-buffer)))
  (if (buffer-file-name)
      (insert
       (file-relative-name filepath (file-name-directory buffer-file-name)))
    (message "Error: (buffer-file-name) returned nil")))

(my/define-leader-key "l i" #'my/buffer-insert-relative-link-to-file)

(defun my/open-file ()
  "Open file.

Credit: xahlee.info"
  (interactive)
  (let ((path (if (eq major-mode 'dired-mode)
                  (if (eq nil (dired-get-marked-files))
                      default-directory
                    (car (dired-get-marked-files)))
                (if buffer-file-name
                    buffer-file-name
                  default-directory))))
    (cond
     ((eq system-type 'windows-nt)
      (shell-command
       (format "PowerShell -Command invoke-item '%s'" (expand-file-name path))))
     ((eq system-type 'darwin)
      (shell-command (concat "open -R " (shell-quote-argument path))))
     (t
      (call-process shell-file-name nil 0 nil
                    shell-command-switch
                    (format "xdg-open '%s'" (expand-file-name path)))))))

(defun my/open-current-directory ()
  "Open the current directory"
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (shell-command
     (format "PowerShell -Command invoke-item '%s'" (expand-file-name default-directory))))
   ((eq system-type 'darwin)
    (shell-command
     (concat "open -R " (shell-quote-argument (expand-file-name default-directory)))))
   (t
    (call-process shell-file-name nil 0 nil
                  shell-command-switch
                  (format "xdg-open '%s'" (expand-file-name default-directory))))))

(defun my/update-dotfiles-dir ()
  "Update sync-ed dotfiles directory at `my/dotfiles-emacs-directory' with
my current emacs configuration in `~/.config/emacs'"
  (interactive)
  (let ((dotfiles-emacs-dir (expand-file-name "emacs/.config/emacs/" my/dotfiles-directory))
        (emacs-gitignore-file (locate-user-emacs-file ".gitignore"))
        (emacs-init-file (locate-user-emacs-file "init.el"))
        (emacs-early-init-file (locate-user-emacs-file "early-init.el")))
    (copy-file emacs-gitignore-file dotfiles-emacs-dir t)
    (copy-file emacs-early-init-file dotfiles-emacs-dir t)
    (copy-file emacs-init-file dotfiles-emacs-dir t)
    (copy-directory my/local-lisp-directory dotfiles-emacs-dir)
    (copy-directory my/local-modules-directory dotfiles-emacs-dir)
    (message "Emacs dotfiles directory updated!")))

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

;;; End
(load (locate-user-emacs-file "machine-init.el") :noerror :nomessage)
