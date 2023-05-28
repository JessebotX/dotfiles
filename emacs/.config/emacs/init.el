;;; -*- lexical-binding: t; -*-

;;; Setup Straight.el for Package Management
(setq straight-check-for-modifications '(check-on-save))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; No littering
(straight-use-package 'no-littering)

;;; Initial emacs setup
(add-to-list 'image-types 'svg)

(defalias 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no

(customize-set-variable 'auto-save-file-name-transforms
                        `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                           ,(concat temporary-file-directory "\\2") t)
                          ("\\`\\(/tmp\\|/dev/shm\\)\\([^/]*/\\)*\\(.*\\)\\'" "\\3")
                          (".*" ,(no-littering-expand-var-file-name "auto-save/") t))) ; Move autosave files somewhere else
(customize-set-variable 'backward-delete-char-untabify-method 'hungry) ; Delete tabs properly
(customize-set-variable 'bidi-inhibit-bpa t)
(customize-set-variable 'create-lockfiles nil)       ; Don't make lock files
(customize-set-variable 'custom-file (no-littering-expand-etc-file-name "custom.el")) ; set custom file to a different location
(customize-set-variable 'global-auto-revert-non-file-buffers t)
(customize-set-variable 'make-backup-files nil)      ; Don't make backup files
(customize-set-variable 'ring-bell-function 'ignore) ; Don't make beep sound (audible bell)
(customize-set-variable 'scroll-conservatively 1001) ; Smoother scrolling
(customize-set-variable 'tab-width 4)                ; Default tab width = 4
(customize-set-variable 'visible-bell nil)           ; Disable visible bell
(customize-set-variable 'whitespace-display-mappings
                        '((tab-mark 9 [124 9] [92 9])))
(customize-set-variable 'whitespace-style '(face tabs tab-mark trailing))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)

(load custom-file 'noerror 'nomessage)

(blink-cursor-mode -1)
(delete-selection-mode 1)
(global-auto-revert-mode)
(global-so-long-mode 1)
(global-whitespace-mode 1)

;;;; Recentf
(recentf-mode 1)
(require 'recentf)
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
(add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory))

;;; Beacon Cursor
(straight-use-package 'beacon)

(beacon-mode 1)

;;; Undo
(straight-use-package 'undo-fu)
(customize-set-variable 'undo-limit 67108864) ; 64mb.
(customize-set-variable 'undo-strong-limit 100663296) ; 96mb.
(customize-set-variable 'undo-outer-limit 1006632960) ; 960mb.

;;; Evil Mode
(straight-use-package 'evil)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-nerd-commenter)

(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-want-C-i-jump nil)
(customize-set-variable 'evil-respect-visual-line-mode t)
(customize-set-variable 'evil-want-C-h-delete t)

(customize-set-variable 'evil-undo-system 'undo-fu)

(evil-mode 1)

(evil-select-search-module 'evil-search-module 'evil-search)
(global-set-key (kbd "C-M-u") 'universal-argument)
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;;;; Visual shifting
(defun my/evil-shift-right ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun my/evil-shift-left ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(evil-define-key 'visual global-map (kbd ">") 'my/evil-shift-right)
(evil-define-key 'visual global-map (kbd "<") 'my/evil-shift-left)

(let ((map evil-insert-state-map))
  (define-key map (kbd "C-g") 'evil-normal-state))

(let ((map evil-normal-state-map))
  (define-key map (kbd "C-h") 'evil-window-left)
  (define-key map (kbd "C-j") 'evil-window-down)
  (define-key map (kbd "C-k") 'evil-window-up)
  (define-key map (kbd "C-l") 'evil-window-right))

(evilnc-default-hotkeys)

(evil-collection-init)

;;; General.el
(straight-use-package 'general)

(general-create-definer my/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")

(my/leader-keys
  "ho" 'describe-symbol
  "hm" 'describe-mode
  "hk" 'describe-key
  "hv" 'describe-variable
  "hf" 'describe-function
  "hi" 'info
  "ti" 'display-line-numbers-mode
  "."  'find-file
  "j"  'execute-extended-command
  "b"  'switch-to-buffer
  "o"  'other-window
  "kk" 'kill-buffer
  "-"  'kill-this-buffer
  "kl" 'delete-other-windows
  "kj" 'delete-window)

;;; Fonts
(straight-use-package 'all-the-icons)
(straight-use-package 'fontaine)

(customize-set-variable 'fontaine-latest-state-file
                       (no-littering-expand-etc-file-name "fontaine-latest-state.eld"))
(customize-set-variable 'fontaine-presets
                       '((regular
                          :default-family "JetBrainsMono Nerd Font"
                          :default-height 150)
                         (jetbrains
                          :default-family "JetBrains Mono NL"
                          :default-height 150)
                         (proggy
                          :default-family "ProggyCleanCE Nerd Font"
                          :default-height 180)
                         (go
                          :default-family "GoMono Nerd Font"
                          :default-height 150)
                         (fira
                          :default-family "FiraMono Nerd Font"
                          :default-height 150)
                         (liberation
                          :default-family "LiterationMono Nerd Font"
                          :default-height 150)
                         (comic_sans
                          :default-family "ComicShannsMono Nerd Font"
                          :default-height 150)
                         (iosevka_comfy_duo
                          :default-family "Iosevka Comfy Duo"
                          :default-height 150)))

(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

(define-key global-map (kbd "C-c f") #'fontaine-set-preset)
(define-key global-map (kbd "C-c F") #'fontaine-set-face-font)

(my/leader-keys
  "tf" 'fontaine-set-preset)

;;; Theme
;;;; Ef themes
(straight-use-package 'ef-themes)

(customize-set-variable 'ef-themes-to-toggle '(ef-light ef-bio))
(customize-set-variable 'ef-themes-headings
                         '((1 . (2.0))
                           (2 . (1.5))
                           (3 . (1.1))))

(my/leader-keys
  "<f5>" 'ef-themes-toggle)

;;;; Modus themes
(straight-use-package 'modus-themes)

(customize-set-variable 'modus-themes-italic-constructs t)
(customize-set-variable 'modus-themes-bold-constructs t)
(customize-set-variable 'modus-themes-mixed-fonts t)
(customize-set-variable 'modus-themes-variable-pitch-ui nil)
(customize-set-variable 'modus-themes-custom-auto-reload t)
(customize-set-variable 'modus-themes-disable-other-themes t)
(customize-set-variable 'modus-themes-headings
                        '((1 . (2.0))
                          (2 . (1.5))
                          (t . (1.1))))

(global-set-key (kbd "<f5>") 'modus-themes-toggle)

;;;; Catppuccin theme
(straight-use-package 'catppuccin-theme)

;;;; Load the theme
(load-theme 'ef-bio t)

;;; Modeline
(straight-use-package 'doom-modeline)
(customize-set-variable 'doom-modeline-height 30)
(customize-set-variable 'doom-modeline-enable-word-count t)
(customize-set-variable 'doom-modeline-icon t)
(doom-modeline-mode 1)

(column-number-mode 1)
(customize-set-variable 'display-time-default-load-average nil)
(display-time-mode 1)

;;; Minibuffer
(defun my/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
      (delete-word (- arg))))

(let ((map minibuffer-local-map))
  (define-key map (kbd "C-<backspace>") #'my/minibuffer-backward-kill)
  (define-key map (kbd "M-<backspace>") #'my/minibuffer-backward-kill))

(straight-use-package 'vertico)

(customize-set-variable 'vertico-cycle t)
(vertico-mode 1)

(let ((map vertico-map))
  (define-key map (kbd "C-j") 'vertico-next)
  (define-key map (kbd "C-k") 'vertico-previous))

;;;; Consult (new interactive commands)
(straight-use-package 'consult)

(my/leader-keys
  "s" 'consult-line
  "rr" 'consult-bookmark
  "rf" 'consult-recent-file
  "tt" 'consult-theme)

;;;; Marginalia (adds helpful info for each entry in the minibuffer)
(straight-use-package 'marginalia)
(marginalia-mode 1)

;;;; Orderless (better minibuffer searching order)
(straight-use-package 'orderless)

(customize-set-variable 'completion-styles '(orderless))
(customize-set-variable 'completion-category-defaults nil)
(customize-set-variable 'completion-category-overrides
                        '((file (styles . (partial-completion)))))

;;; Imenu
(straight-use-package 'imenu-list)

(my/leader-keys
  "ei" 'imenu-list-smart-toggle)

;;; Jinx Spellchecker
(straight-use-package 'jinx)

(my/leader-keys
  "w" 'ispell-word)

(keymap-global-set "<remap> <ispell-word>" #'jinx-correct)

;;; Treemacs
(straight-use-package 'treemacs)

(customize-set-variable 'treemacs-indentation 2)

(my/leader-keys
  "er" 'treemacs
  "ee" 'treemacs-add-and-display-current-project-exclusively)

(require 'treemacs)
(evil-define-key 'normal treemacs-mode-map (kbd "a") 'treemacs-create-file)
(evil-define-key 'normal treemacs-mode-map (kbd "d") 'treemacs-delete)

;;; Dired
(straight-use-package 'dired-single)
(straight-use-package 'all-the-icons-dired)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(customize-set-variable 'dired-hide-details-hide-symlink-targets nil)

(defun my/dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my/dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my/dired-init))

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;;; Vterm
(straight-use-package 'vterm)

;;; Writeroom
(straight-use-package 'olivetti)
(customize-set-variable 'olivetti-body-width 70)

(my/leader-keys
  "tb" 'olivetti-mode)

;;;; Fullscreen writeroom
(straight-use-package 'writeroom-mode)
(customize-set-variable 'writeroom-width 70)

;;; Magit
(straight-use-package 'magit)

(my/leader-keys
  "gg" 'magit)

;;; Denote
(straight-use-package 'denote)

(defconst my/notes-directory "~/Sync/vault/notes/zet")

(customize-set-variable 'denote-directory my/notes-directory)
(customize-set-variable 'denote-file-type 'markdown-yaml)
(customize-set-variable 'denote-yaml-front-matter
                        "---
title:      %1$s
date:       \"%2$s\"
tags:       %3$s
identifier: \"%4$s\"
---
\n")
(add-hook 'dired-mode-hook #'denote-dired-mode)

(defun my/denote-dired-notes ()
  (interactive)
  (require 'dired)
  (dired-jump nil (concat denote-directory "/")))

(my/leader-keys
  "nn" 'denote
  "nr" 'denote-rename-file-using-front-matter
  "nl" #'my/denote-dired-notes)

;;; Which key
(straight-use-package 'which-key)
(which-key-mode 1)
(customize-set-variable 'which-key-idle-delay 0.2)

;;; Editing & Programming
(defun my/indent-with-spaces (width)
  (interactive)
  (setq-local tab-width width)
  (setq-local evil-shift-width width)
  (setq-local indent-tabs-mode nil))

(defun my/indent-with-tabs (width)
  (interactive)
  (setq-local tab-width width)
  (setq-local evil-shift-width width)
  (setq-local indent-tabs-mode t))

;;;; Flycheck
(straight-use-package 'flycheck)

;;;; Rainbow delimiters
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;;; LSP
(straight-use-package 'lsp-mode)
(customize-set-variable 'lsp-keymap-prefix "C-c l")

;;;; Completions with Corfu
(straight-use-package 'corfu)
(customize-set-variable 'corfu-cycle t)
(customize-set-variable 'corfu-auto t)
(customize-set-variable 'corfu-auto-prefix 2)
(customize-set-variable 'corfu-auto-delay 0.0)
(customize-set-variable 'corfu-quit-at-boundary 'separator)
;;(customize-set-variable 'corfu-echo-documentation 0.25)
(customize-set-variable 'corfu-preview-current 'insert)
(customize-set-variable 'corfu-preselect-first nil)

(global-corfu-mode)

(let ((map corfu-map))
  (define-key map (kbd "M-SPC") 'corfu-insert-separator)
  (define-key map (kbd "RET") nil)
  (define-key map (kbd "TAB") 'corfu-next)
  (define-key map (kbd "S-TAB") 'corfu-previous)
  (define-key map (kbd "S-<return>") 'corfu-insert))

;;;; Tree-sitter
(straight-use-package 'tree-sitter)
(global-tree-sitter-mode 1)

(straight-use-package 'tree-sitter-langs)

;;;; HTML/CSS
(setq-default sgml-basic-offset 2)
(setq-default css-indent-offset 2)

(defun my/web-dev-mode-setup ()
  (my/indent-with-spaces 2))

(add-hook 'css-mode-hook #'my/web-dev-mode-setup)
(add-hook 'html-mode-hook #'my/web-dev-mode-setup)

;;;; Templating engines
(straight-use-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

(customize-set-variable 'web-mode-markup-indent-offset 2)
(customize-set-variable 'web-mode-code-indent-offset 2)
(customize-set-variable 'web-mode-css-indent-offset 2)
(add-hook 'web-mode-hook #'my/web-dev-mode-setup)

;;;; Tailwind CSS
(straight-use-package 'lsp-tailwindcss)

;;;; Javascript/Typescript
(defun my/js-ts-mode-hook-setup ()
  (lsp-deferred)
  (my/indent-with-spaces 2))

(setq-default js-indent-level 2)
(add-hook 'js-mode-hook #'my/js-ts-mode-hook-setup)
(add-hook 'js-jsx-mode-hook 'my/js-ts-mode-hook-setup)

(straight-use-package 'typescript-mode)
(setq-default typescript-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'typescript-mode-hook #'my/js-ts-mode-hook-setup)

;;;; C/C++
(defun my/cc-mode-hook-setup ()
  (customize-set-variable 'c-file-style "bsd")
  (my/indent-with-tabs 4)
  (customize-set-variable 'c-basic-offset 4)
  (setq c-basic-offset 4))

(add-hook 'c-mode-hook #'my/cc-mode-hook-setup)
(add-hook 'c++-mode-hook #'my/cc-mode-hook-setup)

;;;; Go
(defun my/go-mode-hook-setup ()
;;  (lsp-deferred)
  (flycheck-mode))

(straight-use-package 'go-mode)
(add-hook 'go-mode-hook #'my/go-mode-hook-setup)

;;;; Lisp
(defun my/lisp-mode-hook-setup ()
  (my/indent-with-spaces 2))

(add-hook 'lisp-mode-hook #'my/lisp-mode-hook-setup)
(add-hook 'emacs-lisp-mode-hook #'my/lisp-mode-hook-setup)

;;;; Python
(defun my/python-mode-hook-setup ()
  (customize-set-variable 'python-indent-offset 4)
  (my/indent-with-spaces 4))

(add-hook 'python-mode-hook #'my/python-mode-hook-setup)

;;;; Text modes
(defun my/text-modes-hook-setup ()
  (jinx-mode) ; Spellcheck
  (my/indent-with-spaces 2) ; 2 space indentation
  (auto-fill-mode 1)
  (visual-line-mode 1)
  (olivetti-mode 1)
  (setq-local fill-column 60))

;;;;; Markdown
(straight-use-package 'markdown-mode)

(add-hook 'markdown-mode-hook #'my/text-modes-hook-setup)
